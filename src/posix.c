/* The interpreter is C89, but C89 has no directories and cannot run a
   program with its output captured. Everything that needs POSIX is in this
   file: directories, file status, the path of a file, and running a
   program. */
#define _XOPEN_SOURCE 700

#include "eval.h"

#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

/* pathKind :: String -> IO Int
   0: there is nothing at the path, 1: a directory, 2: anything else. */
Value *builtin_pathKind(Value **arguments)
{
    struct stat status;

    if (stat(c_string(arguments[0], NULL), &status) != 0) {
        return make_io_result(make_int(0));
    }
    return make_io_result(make_int(S_ISDIR(status.st_mode) ? 1 : 2));
}

/* fileSize :: String -> IO Int        (-1: no such file) */
Value *builtin_fileSize(Value **arguments)
{
    struct stat status;

    if (stat(c_string(arguments[0], NULL), &status) != 0) {
        return make_io_result(make_int(-1));
    }
    return make_io_result(make_int((long)status.st_size));
}

/* isExecutable :: String -> IO Bool */
Value *builtin_isExecutable(Value **arguments)
{
    return make_io_result(
        make_bool(access(c_string(arguments[0], NULL), X_OK) == 0));
}

/* listDirectory :: String -> IO (Maybe [String])
   The names in the directory other than "." and "..", in the order the
   system gives them, which is the order GHC's listDirectory gives too. */
Value *builtin_listDirectory(Value **arguments)
{
    DIR *directory = opendir(c_string(arguments[0], NULL));
    Value *list = make_con(TAG_NIL, 0);
    Value **last = &list;
    struct dirent *entry;

    if (directory == NULL) {
        return make_io_result(make_maybe(NULL));
    }
    while ((entry = readdir(directory)) != NULL) {
        Value *cell;

        if (strcmp(entry->d_name, ".") == 0
            || strcmp(entry->d_name, "..") == 0) {
            continue;
        }
        cell = make_con(TAG_CONS, 2);
        cell->fields[0] = make_string(entry->d_name);
        cell->fields[1] = *last;
        *last = cell;
        last = &cell->fields[1];
    }
    closedir(directory);
    return make_io_result(make_maybe(list));
}

/* makeDirectory :: String -> IO Bool        (False: failed) */
Value *builtin_makeDirectory(Value **arguments)
{
    return make_io_result(
        make_bool(mkdir(c_string(arguments[0], NULL), 0777) == 0));
}

/* removePath :: String -> IO Bool
   Removes a file or an empty directory. False: failed. */
Value *builtin_removePath(Value **arguments)
{
    return make_io_result(
        make_bool(remove(c_string(arguments[0], NULL)) == 0));
}

/* renamePath :: String -> String -> IO Bool        (False: failed) */
Value *builtin_renamePath(Value **arguments)
{
    return make_io_result(make_bool(rename(c_string(arguments[0], NULL),
                                           c_string(arguments[1], NULL)) == 0));
}

/* currentDirectory :: IO String */
Value *builtin_currentDirectory(Value **arguments)
{
    char path[PATH_MAX];

    (void)arguments;
    if (getcwd(path, sizeof(path)) == NULL) {
        die("cannot find the current directory");
    }
    return make_io_result(make_string(path));
}

/* realPath :: String -> IO (Maybe String)
   The path with every symbolic link, "." and ".." resolved. Nothing if a
   part of it does not exist. */
Value *builtin_realPath(Value **arguments)
{
    char resolved[PATH_MAX];

    if (realpath(c_string(arguments[0], NULL), resolved) == NULL) {
        return make_io_result(make_maybe(NULL));
    }
    return make_io_result(make_maybe(make_string(resolved)));
}

/* temporaryFile :: String -> IO String
   Creates a new, empty file in the directory and returns its path. */
Value *builtin_temporaryFile(Value **arguments)
{
    char *directory = c_string(arguments[0], NULL);
    char *path = gc_alloc_bytes(strlen(directory) + 32);
    int descriptor;

    sprintf(path, "%s/aihc-bootstrap-XXXXXX", directory);
    descriptor = mkstemp(path);
    if (descriptor < 0) {
        die("cannot create a temporary file in %s", directory);
    }
    close(descriptor);
    return make_io_result(make_string(path));
}

/* A Haskell list of strings as a NULL-terminated array, with `first` in
   front of it. */
static char **string_array(char *first, Value *list)
{
    char **array = NULL;
    int count = 0;

    array = gc_realloc(array, 2 * sizeof(char *), 0);
    array[count++] = first;
    list = force(list);
    while (list->tag == TAG_CONS) {
        array = gc_realloc(array, ((size_t)count + 2) * sizeof(char *), 0);
        array[count++] = c_string(list->fields[0], NULL);
        list = force(list->fields[1]);
    }
    array[count] = NULL;
    return array;
}

/* Makes the file the standard stream `target` of this process. */
static void redirect(const char *path, int flags, int target)
{
    int descriptor = open(path, flags, 0666);

    if (descriptor < 0 || dup2(descriptor, target) < 0) {
        _exit(127);
    }
    close(descriptor);
}

/* runProcess :: String -> [String] -> String -> String -> String -> String
                 -> IO Int
   Runs the program with the arguments and waits for it. The program is
   looked up in PATH. The other strings are the directory to run it in
   ("" for the current one) and the files that are its standard input,
   output and error; the output files are overwritten. The result is the
   exit status, or minus the number of the signal that ended the program. */
Value *builtin_runProcess(Value **arguments)
{
    char *program = c_string(arguments[0], NULL);
    char **argv = string_array(program, arguments[1]);
    char *directory = c_string(arguments[2], NULL);
    char *input = c_string(arguments[3], NULL);
    char *output = c_string(arguments[4], NULL);
    char *error = c_string(arguments[5], NULL);
    int status = 0;
    pid_t child;

    fflush(stdout);
    fflush(stderr);
    child = fork();
    if (child < 0) {
        die("cannot start %s", program);
    }
    if (child == 0) {
        if (directory[0] != '\0' && chdir(directory) != 0) {
            _exit(127);
        }
        redirect(input, O_RDONLY, 0);
        redirect(output, O_WRONLY | O_CREAT | O_TRUNC, 1);
        redirect(error, O_WRONLY | O_CREAT | O_TRUNC, 2);
        execvp(program, argv);
        _exit(127);
    }
    if (waitpid(child, &status, 0) < 0) {
        die("cannot wait for %s", program);
    }
    if (WIFSIGNALED(status)) {
        return make_io_result(make_int(-(long)WTERMSIG(status)));
    }
    return make_io_result(make_int(WEXITSTATUS(status)));
}

/* sleepMicroseconds :: Int -> IO () */
Value *builtin_sleepMicroseconds(Value **arguments)
{
    long microseconds = force(arguments[0])->number;
    struct timespec duration;

    if (microseconds > 0) {
        duration.tv_sec = (time_t)(microseconds / 1000000);
        duration.tv_nsec = (microseconds % 1000000) * 1000;
        nanosleep(&duration, NULL);
    }
    return make_io_result(make_con(TAG_UNIT, 0));
}

/* monotonicNanoseconds :: IO Word64
   The time since some fixed moment, on a clock that never goes back. */
Value *builtin_monotonicNanoseconds(Value **arguments)
{
    struct timespec now;

    (void)arguments;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return make_io_result(
        make_int((long)now.tv_sec * 1000000000l + (long)now.tv_nsec));
}

/* processId :: IO Int
   A number that no other running program has: for the names of temporary
   files. */
Value *builtin_processId(Value **arguments)
{
    (void)arguments;
    return make_io_result(make_int((long)getpid()));
}

/* environment :: IO [String]
   Every variable of the environment, as NAME=value. */
extern char **environ;

Value *builtin_environment(Value **arguments)
{
    Value *list = make_con(TAG_NIL, 0);
    int count = 0;
    int i;

    (void)arguments;
    while (environ[count] != NULL) {
        count++;
    }
    for (i = count - 1; i >= 0; i--) {
        Value *cell = make_con(TAG_CONS, 2);

        cell->fields[0] = make_string(environ[i]);
        cell->fields[1] = list;
        list = cell;
    }
    return make_io_result(list);
}

/* fileModified :: String -> IO Int
   When the file was last changed, in nanoseconds since 1970, or -1 if it
   is not there. */
Value *builtin_fileModified(Value **arguments)
{
    struct stat status;
    long nanoseconds;

    if (stat(c_string(arguments[0], NULL), &status) != 0) {
        return make_io_result(make_int(-1));
    }
#ifdef __APPLE__
    nanoseconds = (long)status.st_mtimensec;
#else
    nanoseconds = (long)status.st_mtim.tv_nsec;
#endif
    return make_io_result(
        make_int((long)status.st_mtime * 1000000000l + nanoseconds));
}

/* ---- A large stack ---- */

static int (*stack_function)(int, char **);
static int stack_argc;
static char **stack_argv;
static int stack_result;

static void *on_big_stack(void *unused)
{
    (void)unused;
    stack_result = stack_function(stack_argc, stack_argv);
    return NULL;
}

/* The stack is address space, not memory: only the part that is used is
   ever given to the program. */
#define BIG_STACK_BYTES (4ul * 1024 * 1024 * 1024)

int run_with_big_stack(int (*function)(int, char **), int argc, char **argv)
{
    pthread_attr_t attributes;
    pthread_t thread;

    stack_function = function;
    stack_argc = argc;
    stack_argv = argv;
    if (pthread_attr_init(&attributes) != 0
        || pthread_attr_setstacksize(&attributes, BIG_STACK_BYTES) != 0
        || pthread_create(&thread, &attributes, on_big_stack, NULL) != 0) {
        /* No such stack to be had: the ordinary one has to do. */
        return function(argc, argv);
    }
    pthread_join(thread, NULL);
    return stack_result;
}
