#include "eval.h"

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Memory for what the running program makes: values, environments, the
   fields of constructors, byte strings. It is freed when nothing refers
   to it any more.

   The collector marks and sweeps, and never moves anything. It does not
   know what a piece of memory is. To find what is in use it starts from
   the roots and treats every word it sees as a possible pointer: if the
   word is the address of something inside an allocated cell, that cell is
   in use, and its words are looked at in turn. A number that happens to
   look like an address keeps a cell alive for nothing, which is harmless.

   The roots are the C stack (the machine calls itself through builtins,
   and C locals hold values), the registers, and what mark_roots in eval.c
   names: the machine's own stack and the values of top-level variables.

   What the interpreter makes while it reads the program (tokens,
   expressions, types) is not here: that is xmalloc, and stays for good.
   Memory from here may point to that memory; the other way round only
   through a root.

   The allocator: memory comes in chunks. A chunk is cut into cells of one
   size, and each size has a list of free cells. Anything larger than the
   largest size is a chunk of one cell. The chunks are kept sorted by
   address, so that the cell a word points into is found by bisection. */

enum {
    CELL_USED = 1,
    CELL_MARKED = 2,
    CELL_BYTES = 4 /* holds no pointers: it is not looked into */
};

typedef struct Block {
    char *base;
    size_t cell_size;
    size_t cell_count;
    unsigned char *state; /* CELL_... for each cell */
} Block;

static const size_t class_sizes[] = {
    16, 32, 48, 64, 96, 128, 192, 256, 384, 512, 768, 1024, 2048, 4096
};

#define CLASS_COUNT ((int)(sizeof(class_sizes) / sizeof(class_sizes[0])))
#define CHUNK_BYTES ((size_t)1024 * 1024)

static Block **chunks = NULL; /* sorted by base */
static int chunk_count = 0;
static int chunk_capacity = 0;

static void *free_cells[CLASS_COUNT]; /* each cell holds the next one */

static const char *stack_bottom = NULL; /* NULL: collecting is not on yet */
static size_t allocated = 0;            /* bytes handed out since the last collection */
static size_t threshold = (size_t)256 * 1024 * 1024;
static size_t minimum_threshold = (size_t)256 * 1024 * 1024;
static int report = 0;
static unsigned long collections = 0;

static void out_of_memory(void)
{
    die("out of memory");
}

/* The chunk the address is inside, or NULL. */
static Block *find_chunk(const char *address)
{
    int low = 0;
    int high = chunk_count - 1;

    while (low <= high) {
        int middle = (low + high) / 2;
        Block *chunk = chunks[middle];

        if (address < chunk->base) {
            high = middle - 1;
        } else if (address >= chunk->base + chunk->cell_size * chunk->cell_count) {
            low = middle + 1;
        } else {
            return chunk;
        }
    }
    return NULL;
}

/* A new chunk of `cell_count` cells, put in its place in the table. */
static Block *add_chunk(size_t cell_size, size_t cell_count)
{
    Block *chunk = malloc(sizeof(Block));
    int place = 0;

    if (chunk == NULL) {
        out_of_memory();
    }
    chunk->base = malloc(cell_size * cell_count);
    chunk->state = calloc(cell_count, 1);
    if (chunk->base == NULL || chunk->state == NULL) {
        out_of_memory();
    }
    chunk->cell_size = cell_size;
    chunk->cell_count = cell_count;
    if (chunk_count == chunk_capacity) {
        chunk_capacity = chunk_capacity * 2 + 256;
        chunks = realloc(chunks, (size_t)chunk_capacity * sizeof(Block *));
        if (chunks == NULL) {
            out_of_memory();
        }
    }
    while (place < chunk_count && chunks[place]->base < chunk->base) {
        place++;
    }
    memmove(chunks + place + 1, chunks + place,
            (size_t)(chunk_count - place) * sizeof(Block *));
    chunks[place] = chunk;
    chunk_count++;
    return chunk;
}

/* ---- Marking ---- */

typedef struct Pending {
    char *cell;
    size_t size;
} Pending;

/* Cells that are marked and whose words are still to be looked at. */
static Pending *pending = NULL;
static size_t pending_count = 0;
static size_t pending_capacity = 0;

void gc_mark(const void *pointer)
{
    Block *chunk = find_chunk(pointer);
    size_t index;

    if (chunk == NULL) {
        return;
    }
    index = (size_t)((const char *)pointer - chunk->base) / chunk->cell_size;
    if ((chunk->state[index] & (CELL_USED | CELL_MARKED)) != CELL_USED) {
        return;
    }
    chunk->state[index] |= CELL_MARKED;
    if (chunk->state[index] & CELL_BYTES) {
        return;
    }
    if (pending_count == pending_capacity) {
        pending_capacity = pending_capacity * 2 + 4096;
        pending = realloc(pending, pending_capacity * sizeof(Pending));
        if (pending == NULL) {
            out_of_memory();
        }
    }
    pending[pending_count].cell = chunk->base + index * chunk->cell_size;
    pending[pending_count].size = chunk->cell_size;
    pending_count++;
}

/* Every word from `start` up to `end` is a possible pointer. */
void gc_mark_range(const void *start, const void *end)
{
    const char *p = start;
    /* Words are looked for where a pointer can be stored. */
    size_t misaligned = (size_t)p % sizeof(void *);

    if (misaligned != 0) {
        p += sizeof(void *) - misaligned;
    }
    for (; p + sizeof(void *) <= (const char *)end; p += sizeof(void *)) {
        void *word;

        memcpy(&word, p, sizeof(word));
        gc_mark(word);
    }
}

static void mark_from_roots(void)
{
    /* The registers may hold the only pointer to something. setjmp writes
       them into this buffer, which is on the stack and so is scanned. */
    jmp_buf registers;
    const char *top = (const char *)&registers;

    memset(&registers, 0, sizeof(registers));
    (void)setjmp(registers);
    mark_roots();
    if (top < stack_bottom) {
        gc_mark_range(top, stack_bottom);
    } else {
        gc_mark_range(stack_bottom, top + sizeof(registers));
    }
    while (pending_count > 0) {
        Pending cell = pending[--pending_count];

        gc_mark_range(cell.cell, cell.cell + cell.size);
    }
}

/* ---- Sweeping ---- */

/* Frees every cell that is not marked, and puts the free cells of each
   size back on its list. Returns how many bytes are still in use. */
static size_t sweep(void)
{
    size_t live = 0;
    int kept = 0;
    int i;
    int k;

    for (k = 0; k < CLASS_COUNT; k++) {
        free_cells[k] = NULL;
    }
    for (i = 0; i < chunk_count; i++) {
        Block *chunk = chunks[i];
        size_t index;
        int size_class = -1;

        for (k = 0; k < CLASS_COUNT; k++) {
            if (class_sizes[k] == chunk->cell_size && chunk->cell_count > 1) {
                size_class = k;
            }
        }
        if (size_class < 0) {
            /* One large cell: the whole chunk goes if it is not in use. */
            if (chunk->state[0] & CELL_MARKED) {
                chunk->state[0] &= (unsigned char)~CELL_MARKED;
                live += chunk->cell_size;
                chunks[kept++] = chunk;
            } else {
                free(chunk->base);
                free(chunk->state);
                free(chunk);
            }
            continue;
        }
        for (index = 0; index < chunk->cell_count; index++) {
            if (chunk->state[index] & CELL_MARKED) {
                chunk->state[index] &= (unsigned char)~CELL_MARKED;
                live += chunk->cell_size;
            } else {
                char *cell = chunk->base + index * chunk->cell_size;

                chunk->state[index] = 0;
                memcpy(cell, &free_cells[size_class], sizeof(void *));
                free_cells[size_class] = cell;
            }
        }
        chunks[kept++] = chunk;
    }
    chunk_count = kept;
    return live;
}

void gc_collect(void)
{
    size_t live;

    if (stack_bottom == NULL) {
        return;
    }
    mark_from_roots();
    live = sweep();
    collections++;
    if (report) {
        fprintf(stderr, "gc %lu: %lu MB in use, %lu MB handed out since the last\n",
                collections, (unsigned long)(live / (1024 * 1024)),
                (unsigned long)(allocated / (1024 * 1024)));
    }
    allocated = 0;
    /* Collect again when as much has been handed out as is in use now, so
       that the work of collecting stays in proportion. */
    threshold = live > minimum_threshold ? live : minimum_threshold;
}

/* ---- Allocating ---- */

static void *allocate(size_t size, int flags)
{
    Block *chunk;
    char *cell;
    size_t index;
    int size_class = 0;

    if (allocated >= threshold) {
        gc_collect();
    }
    if (size == 0) {
        size = 1;
    }
    while (size_class < CLASS_COUNT && class_sizes[size_class] < size) {
        size_class++;
    }
    if (size_class == CLASS_COUNT) {
        /* Too large for a class: a chunk of its own, rounded up to whole
           words. */
        size = (size + sizeof(void *) - 1) / sizeof(void *) * sizeof(void *);
        chunk = add_chunk(size, 1);
        chunk->state[0] = (unsigned char)(CELL_USED | flags);
        memset(chunk->base, 0, size);
        allocated += size;
        return chunk->base;
    }
    if (free_cells[size_class] == NULL) {
        /* A new chunk of this size, all of its cells free. */
        size_t count = CHUNK_BYTES / class_sizes[size_class];

        chunk = add_chunk(class_sizes[size_class], count);
        for (index = 0; index < count; index++) {
            cell = chunk->base + index * chunk->cell_size;
            memcpy(cell, &free_cells[size_class], sizeof(void *));
            free_cells[size_class] = cell;
        }
    }
    cell = free_cells[size_class];
    memcpy(&free_cells[size_class], cell, sizeof(void *));
    chunk = find_chunk(cell);
    index = (size_t)(cell - chunk->base) / chunk->cell_size;
    chunk->state[index] = (unsigned char)(CELL_USED | flags);
    memset(cell, 0, chunk->cell_size);
    allocated += chunk->cell_size;
    return cell;
}

void *gc_alloc(size_t size)
{
    return allocate(size, 0);
}

void *gc_alloc_bytes(size_t size)
{
    return allocate(size, CELL_BYTES);
}

/* The memory made at least `size` bytes long, keeping what is in it. `old`
   may be NULL; then `bytes` says whether the new memory holds no
   pointers. */
void *gc_realloc(void *old, size_t size, int bytes)
{
    Block *chunk;
    size_t index;
    void *new;

    if (old == NULL) {
        return allocate(size, bytes ? CELL_BYTES : 0);
    }
    chunk = find_chunk(old);
    if (chunk == NULL) {
        die("gc_realloc was given memory that is not the collector's");
    }
    if (size <= chunk->cell_size) {
        return old;
    }
    index = (size_t)((char *)old - chunk->base) / chunk->cell_size;
    new = allocate(size, chunk->state[index] & CELL_BYTES);
    /* `old` is still in use: the caller holds it. */
    memcpy(new, old, chunk->cell_size);
    return new;
}

/* Turns collecting on. `bottom` is an address in the frame of the function
   that everything else is called from. The environment can ask for a
   report of every collection (AIHC_GC_REPORT) and for collecting after
   every so many bytes (AIHC_GC_EVERY), which finds mistakes sooner. */
void gc_start(const void *bottom)
{
    const char *every = getenv("AIHC_GC_EVERY");

    stack_bottom = bottom;
    report = getenv("AIHC_GC_REPORT") != NULL;
    if (every != NULL) {
        minimum_threshold = (size_t)strtoul(every, NULL, 10);
        threshold = minimum_threshold;
    }
}
