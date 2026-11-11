#ifndef SYNTAX_H
#define SYNTAX_H

#include "lexer.h"

struct Value; /* eval.h */

typedef struct Expr Expr;
typedef struct Pattern Pattern;
typedef struct Alternative Alternative;
typedef struct Guard Guard;
typedef struct Qualifier Qualifier;
typedef struct Binding Binding;
typedef struct Type Type;
typedef struct Scheme Scheme;
typedef struct TypeCon TypeCon;
typedef struct Class Class;
typedef struct Instance Instance;
typedef struct FamilyInstance FamilyInstance;
typedef struct Chunk Chunk;
typedef struct Global Global;
typedef struct Constructor Constructor;
typedef struct Fixity Fixity;
typedef struct Entity Entity;
typedef struct ImportItem ImportItem;
typedef struct Import Import;
typedef struct Module Module;

/* ---- Types ---- */

enum TypeKind {
    TY_META,  /* not known yet; inference fills in `link` */
    TY_RIGID, /* a type variable bound by a forall */
    TY_CON,   /* Int, Maybe, -> */
    TY_APP    /* Maybe Int is (Maybe) applied to (Int) */
};

struct Type {
    enum TypeKind kind;
    Type *link;           /* TY_META: the type it stands for, or NULL */
    int level;            /* TY_META: how many lets deep it was made, which
                             decides what a let may generalise */
    Binding *variable;    /* TY_RIGID */
    TypeCon *constructor; /* TY_CON */
    Type *function;       /* TY_APP */
    Type *argument;       /* TY_APP */
};

/* forall variables. type */
struct Scheme {
    int variable_count;
    Binding **variables;
    Type *type;
};

/* A type constructor: declared by data or newtype, or a type synonym. */
struct TypeCon {
    const char *name;
    Module *module; /* NULL for the built-in ones: ->, [] and () */
    int parameter_count;
    Binding **parameters;
    int is_synonym;
    int rhs_start;  /* synonym: the tokens of the right-hand side */
    int rhs_end;
    Type *rhs;      /* synonym: NULL until first use */
    int constructor_count; /* data, newtype */
    Constructor **constructors;
    int is_family;  /* declared with `type F a` in a class: see below */
};

/* An associated type. A class declares it:

       class Stream s where
         type Token s

   and each instance says what it is for its type:

       instance Stream Text where
         type Token Text = Char

   `Token s` stays as it is written until `s` is known, which at the latest
   is when the program runs. Then it is replaced by the right-hand side of
   the instance whose left-hand side it matches (reduce_family). */
struct FamilyInstance {
    Module *module;
    int lhs_start; /* the tokens of `Token Text` */
    int lhs_end;
    int rhs_start; /* the tokens of `Char` */
    int rhs_end;
    Type *lhs;     /* NULL until first use; then the next three are set */
    Type *rhs;
    int variable_count;
    Binding **variables;
    FamilyInstance *next; /* all of them, of all loaded modules */
};

/* class C a where ... (one parameter only, for now) */
struct Class {
    const char *name;
    Module *module;
    Binding *variable;
};

/* instance C (T a b) where ...
   Instances are found at run time by matching `head` against the type a
   method is used at, so contexts and superclasses play no role. */
struct Instance {
    Module *module;
    int class_token; /* the token that names the class */
    int head_start;  /* the tokens of the type after the class name */
    int head_end;
    Class *class;    /* NULL until first use; then the next three are set */
    int variable_count;
    Binding **variables;
    Type *head;
    Global *methods; /* the method definitions, linked by `next` */
    Instance *next;  /* all instances of all loaded modules */
    /* An instance from a `deriving` clause has no definitions. Its head is
       the type the clause is on, and its methods are made by derive.c. */
    TypeCon *derived_type; /* that type, else NULL */
    int derived_strategy;  /* DERIVE_... */
};

/* deriving [stock | newtype | anyclass] (C) [via T] */
enum {
    DERIVE_DEFAULT, /* no strategy named: derive.c decides as GHC does */
    DERIVE_STOCK,
    DERIVE_NEWTYPE,
    DERIVE_ANYCLASS,
    DERIVE_VIA
};

/* ---- Expressions ---- */

/* A local variable or a type variable. Two variables are the same variable
   exactly when they are the same Binding, so no renaming to unique strings
   is needed. */
struct Binding {
    const char *name;
    Type *type; /* of a local variable, during type inference */

    /* The rest is for a variable bound by let or where. */
    Expr **definition;   /* where its definition is kept, else NULL */
    int is_function;     /* defined with parameters: f x = ... */
    int signature_start; /* the tokens of its type signature, or 0 and 0 */
    int signature_end;
    Scheme *scheme;      /* its type if it is polymorphic, else NULL */
    int level;           /* of the let, for type inference */
    int state;           /* of type inference: see infer.c */
    Expr **uses;         /* uses seen while its own type is being inferred */
    int use_count;
    /* Local functions that call each other are generalised together: see
       infer_binding. */
    Binding *outer;      /* the function whose definition was being inferred
                            when this one's was started, or NULL */
    Binding *leader;     /* a function further out that this one has to be
                            generalised with, or NULL */
    Binding **members;   /* the functions to generalise with this one */
    int member_count;
};

/* A data constructor. */
struct Constructor {
    const char *name;
    TypeCon *type;
    Module *module; /* NULL for the built-in ones: (), [] and : */
    int tag;        /* index of the constructor in its type */
    int arity;
    int is_newtype; /* no run-time representation: N x is the same as x */
    int is_infix;   /* declared between its two fields: a :| b */
    int *field_starts; /* the tokens of the type of each field */
    int *field_ends;
    int *field_strict; /* whether the field is evaluated on construction */
    const char **field_names; /* NULL for a field without a name */
    Scheme *scheme; /* NULL until first use */
    /* data T = forall e. C e: the type variables of the constructor itself.
       A constructed value stores their types in front of its fields, so
       that a pattern match can bring them back. */
    int existential_count;
    Binding **existentials;
};

enum ExprKind {
    E_NAME,        /* a variable the parser saw; the resolver replaces it */
    E_LOCAL,       /* a lambda- or pattern-bound variable */
    E_GLOBAL,      /* a top-level variable of some module */
    E_CONSTRUCTOR, /* `constructor` is NULL until resolved */
    E_INT,
    E_CHAR,
    E_STRING,
    E_APP,
    E_LAMBDA,
    E_CASE,
    E_LET,         /* let, where and the let of a do block. The bindings may
                      refer to each other and to themselves */
    E_OPERATORS,   /* a + b * c before precedence is known; the resolver
                      replaces it by applications */
    E_RECORD,      /* C { f = a } or e { f = a }; the resolver replaces it */
    E_ANNOTATION,  /* body :: type */
    E_TYPE,        /* a type passed as an argument; made by the type pass */
    E_METHOD       /* a class method at a type; made by the type pass */
};

/* Every kind uses only some of the fields. One flat struct is easier to read
   than a union, and we do not care about the wasted memory. */
struct Expr {
    enum ExprKind kind;
    const Token *token;        /* where the expression starts */
    const char *qualifier;     /* E_NAME: the M of M.x, or NULL */
    const char *name;          /* E_NAME, E_CONSTRUCTOR */
    Binding *local;            /* E_LOCAL; the parameter of an E_LAMBDA */
    Global *global;            /* E_GLOBAL; the method of an E_METHOD */
    Constructor *constructor;  /* E_CONSTRUCTOR */
    long number;               /* E_INT, E_CHAR */
    int big;                   /* E_INT: see Token.big */
    const char *string;        /* E_STRING */
    Expr *function;            /* E_APP; E_RECORD: the C or e before { */
    Expr *argument;            /* E_APP */
    Expr *body;                /* E_LAMBDA, E_ANNOTATION, E_LET */
    int binding_count;         /* E_LET */
    Binding **bindings;        /* E_LET */
    Expr **values;             /* E_LET: the definition of each binding */
    Expr *scrutinee;           /* E_CASE */
    Alternative *alternatives; /* E_CASE */
    int operator_count;        /* E_OPERATORS */
    Expr **operators;          /* E_OPERATORS: operator_count of them */
    Expr **operands;           /* E_OPERATORS: operator_count + 1 of them */
    int *negated;              /* E_OPERATORS: whether each operand has a
                                  minus sign in front */
    int field_count;           /* E_RECORD */
    const char **field_names;  /* E_RECORD */
    const char *field_qualifier; /* E_RECORD: the M of the first M.f, or NULL */
    Expr **field_values;       /* E_RECORD: NULL for a field given as { f } */
    int field_wildcard;        /* E_RECORD: { .. } */
    int type_start;            /* E_ANNOTATION: the tokens of the type */
    int type_end;
    Type *type;                /* E_TYPE; E_METHOD: the class argument */
};

/* A qualifier that makes a name refer to the Prelude whatever is in scope.
   The parser uses it for the names that syntax stands for, such as the >>=
   of a do block. Compare by pointer. */
extern const char *const PRELUDE_NAME;

enum PatternKind {
    P_VARIABLE,
    P_WILDCARD,
    P_CONSTRUCTOR,
    P_CHAR,
    P_LITERAL, /* a number, or a string under OverloadedStrings: matches a
                  value that is == to it */
    P_RECORD,  /* C { f = p, g, .. }; the resolver makes it a P_CONSTRUCTOR */
    P_AS,      /* variable@pattern */
    P_BANG,    /* !pattern: evaluates the value before matching */
    P_VIEW     /* (function -> pattern): matches if the function applied to
                  the value matches the pattern */
};

struct Pattern {
    enum PatternKind kind;
    const Token *token;
    Binding *variable;        /* P_VARIABLE, P_AS */
    Pattern *inner;           /* P_AS, P_BANG, P_VIEW */
    const char *qualifier;    /* P_CONSTRUCTOR: PRELUDE_NAME or NULL */
    const char *name;         /* P_CONSTRUCTOR, P_RECORD */
    Constructor *constructor; /* P_CONSTRUCTOR: NULL until resolved */
    int argument_count;       /* P_CONSTRUCTOR */
    Pattern **arguments;      /* P_CONSTRUCTOR */
    Binding **existentials;   /* P_CONSTRUCTOR: variables for the types the
                                 value stores; set by the type pass */
    long number;              /* P_CHAR */
    int field_count;          /* P_RECORD */
    const char **field_names; /* P_RECORD */
    Pattern **field_patterns; /* P_RECORD: NULL for a field given as { f } */
    int field_wildcard;       /* P_RECORD: { .. } */
    Expr *literal;            /* P_LITERAL; P_VIEW: the function */
    Expr *equals;             /* P_LITERAL: == at the type of the literal;
                                 set by the type pass */
};

/* One alternative of a case expression:

       pattern -> body
       pattern | guard -> body | guard -> body  where bindings

   With guards, the alternative is chosen only if one of them holds;
   otherwise the next alternative is tried. The clauses of a function are
   alternatives too (parse.c). */
struct Alternative {
    Pattern *pattern;
    Expr *body;    /* NULL if there are guards */
    Guard *guards;
    Expr *where;   /* an E_LET without a body, or NULL. Its bindings are
                      in scope in the guards and the bodies */
    Alternative *next;
};

/* | qualifier, qualifier -> body */
struct Guard {
    Qualifier *qualifiers;
    Expr *body;
    Guard *next;
};

/* A condition, or `pattern <- expr`, which holds if the pattern matches and
   then binds its variables for what follows. */
struct Qualifier {
    Pattern *pattern; /* NULL for a condition */
    Expr *expr;
    Qualifier *next;
};

/* ---- Modules ---- */

/* The tokens [start, end) of one declaration. */
struct Chunk {
    int start;
    int end;
    Chunk *next;
};

/* A top-level variable, a class method, or the definition of a method in an
   instance. Loading a module only records where the clauses are. They are
   parsed, resolved and typed the first time the variable is needed
   (elaborate.c), so code AIHC never runs costs nothing. */
struct Global {
    const char *name;
    Module *module;
    Chunk *clauses;       /* defining clauses, in source order. For a class
                             method: the default definition, if any */
    int signature_start;  /* the tokens of the type after ::, or 0 and 0 */
    int signature_end;
    const char *builtin;  /* for `foreign import prim`, else NULL */
    Class *class;         /* for a class method, else NULL */
    TypeCon *selector;    /* for a record field: its type, else NULL. The
                             definition is made by elaborate.c */
    Instance *instance;   /* for a definition in an instance, else NULL */
    Scheme *scheme;       /* NULL until first needed */
    Expr *expr;           /* the elaborated definition; NULL until needed */
    int elaborating;
    int failed;           /* survey mode: elaborating it failed */
    struct Value *value;  /* NULL until first use */
    Global *next;         /* the other definitions of the same instance */
};

enum Associativity {
    ASSOC_LEFT,
    ASSOC_RIGHT,
    ASSOC_NONE
};

/* infixl 6 + */
struct Fixity {
    const char *name;
    enum Associativity associativity;
    int precedence;
    Fixity *next;
};

/* Variables, constructors and types have separate sets of names. Classes
   share the names of types. */
enum Namespace {
    NS_VARIABLE,
    NS_CONSTRUCTOR,
    NS_TYPE
};

/* Something a module defines under a name. Exactly one of the four pointers
   is set. */
struct Entity {
    enum Namespace namespace;
    const char *name;
    const char *parent; /* the T or C that `T (..)` or `C (..)` in an import
                           list stands for: the type of a constructor, the
                           class of a method; else NULL */
    Global *global;
    Constructor *constructor;
    TypeCon *type;
    Class *class;
    Entity *next;
};

/* One name in an import list. */
struct ImportItem {
    const char *name;
    int all_children; /* T (..) */
    ImportItem *next;
};

/* import qualified Data.Map as Map hiding (foldr) */
struct Import {
    const char *name;
    const char *alias;  /* the name after `as`, else the module name */
    int qualified;
    int has_list;
    int hiding;
    ImportItem *items;
    Module *module;     /* NULL until a name is first looked up through it */
    int attempted;      /* survey mode has tried to load it */
    Import *next;
};

struct Module {
    const char *name;
    Token *tokens;
    Entity *entities;
    Fixity *fixities;
    Import *imports;
    int reexport_count;     /* module M (module A, module B) where */
    const char **reexports; /* the A and B: module names or import aliases */
    Import *exports;        /* the names of the export list, kept like an
                               import list; NULL if there is no list */
    Chunk *unsupported; /* declarations we skip */
    int overloaded_strings; /* the OverloadedStrings extension is on */
    unsigned long searched; /* the last search of exports that came here */
    Module *next;       /* all loaded modules */
};

/* module.c */
extern TypeCon function_type_constructor; /* -> */
extern TypeCon list_type_constructor;     /* [] */
extern TypeCon unit_type_constructor;     /* () */
extern Constructor unit_constructor;      /* () */
extern Constructor nil_constructor;       /* [] */
extern Constructor cons_constructor;      /* : */
extern Instance *all_instances;
extern FamilyInstance *all_family_instances;

/* Tuples of 2 to MAX_TUPLE elements. The constructor and the type are both
   named (,) or (,,) and so on. */
#define MAX_TUPLE 15
const char *tuple_name(int arity);
TypeCon *tuple_type_constructor(int arity);

/* Modules found in the directory have OverloadedStrings on if
   `overloaded_strings`, as a package can ask for in its .cabal file. */
void add_search_directory(const char *directory, int overloaded_strings);
/* Something a named module of the library defines. Exits if there is no
   such thing: the interpreter itself depends on it. */
Entity *library_entity(const char *module, enum Namespace namespace,
                       const char *name);
Module *load_module_file(const char *path);
Module *load_module(const char *name);
/* Loads every module that a loaded module imports. Returns 0 if there was
   nothing left to load. */
int load_all_imports(void);
/* The same, one import at a time and each import only once, so that a
   module that cannot be loaded does not stop the others. */
int load_next_import(void);
/* The loaded modules, linked by `next`. */
Module *first_loaded_module(void);
/* Something the module itself defines, or NULL. */
Entity *find_entity(Module *module, enum Namespace namespace, const char *name);
/* Something an importer of the module can see: what it defines, what the
   modules it re-exports define, and the imported names in its export list.
   The export list does not hide anything the module defines. */
Entity *find_exported_entity(Module *module, enum Namespace namespace,
                             const char *name);
/* What a name means inside the module, or NULL. `qualifier` is the M of M.x,
   NULL, or PRELUDE_NAME. */
Entity *lookup_entity(Module *module, enum Namespace namespace,
                      const char *qualifier, const char *name);
/* The fixity declared for the name in the module, or NULL. */
const Fixity *find_fixity(const Module *module, const char *name);
/* Says on stderr which declarations of the module were skipped, if any. */
void note_unsupported(const Module *module);
/* Reports a name that means nothing in the module, and exits. */
void unresolved(Module *module, const Token *token, const char *name);
/* Splits Map.Bin into the qualifier Map (NULL if there is none) and Bin. */
const char *split_qualified(const char *name, const char **qualifier);

/* parse.c */
Expr *parse_global(Global *global);

/* resolve.c: replaces every name in place by what it refers to, and gives
   operators their precedence. */
void resolve(Module *module, Expr *expr);

/* types.c */
Binding *new_binding(const char *name);
Type *new_meta(void);
Type *new_rigid(Binding *variable);
Type *new_type_constructor(TypeCon *constructor);
Type *new_type_application(Type *function, Type *argument);
Type *function_type(Type *argument, Type *result);
/* The type with every filled-in TY_META at its top replaced by what it
   stands for. */
Type *prune(Type *type);
/* If the type is `argument -> result`, returns 1 and sets both. */
int split_function_type(Type *type, Type **argument, Type **result);
/* The type with each of the variables replaced. */
Type *substitute(Type *type, int count, Binding **variables, Type **types);
void print_type(Type *type); /* on stderr */
/* Whether the two types are the same and fully known. */
int types_equal(Type *a, Type *b);
/* Parses the tokens [start, end) of the module as a type. `*variables` and
   `*count` are the type variables in scope. If `open`, unknown variables are
   added to them: that is how `a -> a` means `forall a. a -> a`. */
Type *parse_type(Module *module, int start, int end, Binding ***variables,
                 int *count, int open);
Scheme *global_scheme(Global *global);
Scheme *constructor_scheme(Constructor *constructor);
/* Fills in the class and head of the instance. */
void prepare_instance(Instance *instance);

/* instances.c: if the type is an associated type applied to a type that an
   instance covers, what the instance says it is. Otherwise the type. */
Type *reduce_family(Type *type);
/* Whether the type is an associated type applied to something. */
int is_family_application(Type *type);

/* infer.c: works out the types in the definition of the variable, makes type
   arguments explicit, and returns the definition as a function of the
   variable's own type variables. Sets global->scheme if it had no
   signature. */
Expr *infer_global(Global *global, Expr *expr);

/* elaborate.c: makes sure global->expr is set. */
void elaborate_global(Global *global);
/* After a failure that a failure handler caught: forgets the definitions
   that were being elaborated, and marks the innermost one as failed. */
void abandon_elaboration(void);

#endif
