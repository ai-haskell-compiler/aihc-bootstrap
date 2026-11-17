#include "eval.h"

#include <string.h>

/* Derived instances. A `deriving` clause does not generate code. Its
   instance has methods that are the builtins below, each given the type it
   works at. They need nothing else, because every value has the same shape
   (a tag and fields) and the types of the fields can be worked out from
   the type: == compares the tags and then calls == at the type of each
   field, so an instance a field type defines for itself is respected.

   The methods a builtin does not cover come from the class's defaults:
   Ord's < from compare, Show's show from showsPrec, and so on. */

#define TAG_LT 0
#define TAG_EQ 1
#define TAG_GT 2

/* ---- Types and fields ---- */

/* The type constructor a type is an application of: T for T a b. */
static TypeCon *type_head(Type *type)
{
    type = prune(type);
    while (type->kind == TY_APP) {
        type = prune(type->function);
    }
    if (type->kind != TY_CON) {
        die("a derived method was used at a type that is not known");
    }
    return type->constructor;
}

/* The type of field `index` of the constructor, in a value of type `type`:
   the declared field type with the parameters of T replaced by the
   arguments T has in `type`. */
static Type *field_type(Type *type, Constructor *constructor, int index)
{
    TypeCon *head = constructor->type;
    Scheme *scheme = constructor_scheme(constructor);
    Type **arguments =
        gc_alloc((size_t)head->parameter_count * sizeof(Type *) + 1);
    Type *fields;
    Type *field = NULL;
    Type *rest;
    int i;

    type = prune(type);
    for (i = head->parameter_count - 1; i >= 0; i--) {
        arguments[i] = type->argument;
        type = prune(type->function);
    }
    fields = substitute(scheme->type, head->parameter_count, scheme->variables,
                        arguments);
    for (i = 0; i <= index; i++) {
        if (!split_function_type(fields, &field, &rest)) {
            die("constructor %s has no field %d", constructor->name, index);
        }
        fields = rest;
    }
    return field;
}

/* The constructor of the value and its fields. A newtype has no
   representation of its own: its one field is the value itself. */
static Constructor *constructor_of(Type *type, Value **value, Value ***fields)
{
    TypeCon *head = type_head(type);

    if (head->constructor_count == 0) {
        die("a derived method was used at %s, which has no constructors",
            head->name);
    }
    if (head->constructors[0]->is_newtype) {
        *fields = value;
        return head->constructors[0];
    }
    *value = force(*value);
    *fields = (*value)->b.fields;
    return head->constructors[(*value)->tag];
}

/* The Prelude's method of that name. The derived methods ask for one at
   every call, so what was found is remembered, by the name's address:
   the names are string constants. */
static Global *prelude_method(const char *name)
{
    static const char *names[64];
    static Global *globals[64];
    static int count = 0;
    Global *global;
    int i;

    for (i = 0; i < count; i++) {
        if (names[i] == name) {
            return globals[i];
        }
    }
    global = library_entity("Prelude", NS_VARIABLE, name)->global;
    if (count < 64) {
        names[count] = name;
        globals[count] = global;
        count++;
    }
    return global;
}

/* The builtin with the type as its first argument. */
static Value *at_type(const Builtin *builtin, Type *type)
{
    return apply(make_builtin(builtin), make_type(type));
}

/* ---- Eq and Ord ---- */

/* derivedEq :: a -> a -> Bool */
static Value *derived_eq(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;
    Value *a = arguments[1];
    Value *b = arguments[2];
    Value **a_fields;
    Value **b_fields;
    Constructor *constructor = constructor_of(type, &a, &a_fields);
    int i;

    if (constructor != constructor_of(type, &b, &b_fields)) {
        return make_con(TAG_FALSE, 0);
    }
    for (i = 0; i < constructor->arity; i++) {
        Value *equals =
            find_method(prelude_method("=="), field_type(type, constructor, i));

        if (apply(apply(equals, a_fields[i]), b_fields[i])->tag != TAG_TRUE) {
            return make_con(TAG_FALSE, 0);
        }
    }
    return make_con(TAG_TRUE, 0);
}

/* derivedCompare :: a -> a -> Ordering
   Constructors compare by their position in the declaration, and values of
   the same constructor by their fields from left to right. */
static Value *derived_compare(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;
    Value *a = arguments[1];
    Value *b = arguments[2];
    Value **a_fields;
    Value **b_fields;
    Constructor *constructor = constructor_of(type, &a, &a_fields);
    Constructor *other = constructor_of(type, &b, &b_fields);
    int i;

    if (constructor != other) {
        return make_con(constructor->tag < other->tag ? TAG_LT : TAG_GT, 0);
    }
    for (i = 0; i < constructor->arity; i++) {
        Value *compare = find_method(prelude_method("compare"),
                                     field_type(type, constructor, i));
        Value *order = apply(apply(compare, a_fields[i]), b_fields[i]);

        if (order->tag != TAG_EQ) {
            return order;
        }
    }
    return make_con(TAG_EQ, 0);
}

/* ---- Show ---- */

/* The C string in front of the Haskell string. */
static Value *prepend(const char *text, Value *rest)
{
    Value *list = rest;
    Value **last = &list;

    while (*text != '\0') {
        Value *cell = make_con(TAG_CONS, 2);

        cell->b.fields[0] = make_char(utf8_decode(&text));
        cell->b.fields[1] = rest;
        *last = cell;
        last = &cell->b.fields[1];
    }
    return list;
}

/* A name as it is shown: an operator goes in parentheses. */
static Value *prepend_name(const char *name, Value *rest)
{
    int is_operator = strchr("!#$%&*+./<=>?@\\^|-~:", name[0]) != NULL;

    if (is_operator) {
        rest = prepend(")", rest);
    }
    rest = prepend(name, rest);
    return is_operator ? prepend("(", rest) : rest;
}

/* showsPrec precedence value rest, at the type of a field. */
static Value *show_field(Type *type, long precedence, Value *value, Value *rest)
{
    Value *shows = find_method(prelude_method("showsPrec"), type);

    return apply(apply(apply(shows, make_int(precedence)), value), rest);
}

/* derivedShowsPrec :: Int -> a -> String -> String
   As GHC shows a value:
       C                     a constructor without fields
       C a b                 in parentheses where an argument is expected
       C {f = a, g = b}      a record, likewise
       a :| b                an infix constructor, in parentheses where
                             something that binds tighter is expected */
static Value *derived_showsPrec(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;
    long precedence = force(arguments[1])->a.number;
    Value *value = arguments[2];
    Value *rest = arguments[3];
    Value **fields;
    Constructor *constructor = constructor_of(type, &value, &fields);
    int is_record = constructor->arity > 0 && constructor->field_names[0] != NULL;
    int parentheses = constructor->arity > 0 && precedence > 10;
    int i;

    if (constructor->is_infix) {
        const Fixity *fixity = find_fixity(constructor->module, constructor->name);
        long own = fixity != NULL ? fixity->precedence : 9;

        parentheses = precedence > own;
        if (parentheses) {
            rest = prepend(")", rest);
        }
        rest = show_field(field_type(type, constructor, 1), own + 1, fields[1],
                          rest);
        rest = prepend(" ", rest);
        rest = prepend(constructor->name, rest);
        rest = prepend(" ", rest);
        rest = show_field(field_type(type, constructor, 0), own + 1, fields[0],
                          rest);
        return parentheses ? prepend("(", rest) : rest;
    }

    /* Built from the end backwards. */
    if (parentheses) {
        rest = prepend(")", rest);
    }
    if (is_record) {
        rest = prepend("}", rest);
    }
    for (i = constructor->arity - 1; i >= 0; i--) {
        Type *field = field_type(type, constructor, i);

        if (is_record) {
            rest = show_field(field, 0, fields[i], rest);
            rest = prepend(" = ", rest);
            rest = prepend_name(constructor->field_names[i], rest);
            rest = prepend(i > 0 ? ", " : " {", rest);
        } else {
            rest = show_field(field, 11, fields[i], rest);
            rest = prepend(" ", rest);
        }
    }
    rest = prepend_name(constructor->name, rest);
    return parentheses ? prepend("(", rest) : rest;
}

/* ---- Enum and Bounded ---- */

/* Derived Enum is for types whose constructors have no fields: the tag of
   a value is its number. */

/* derivedToEnum :: Int -> a */
static Value *derived_toEnum(Value **arguments)
{
    TypeCon *head = type_head(force(arguments[0])->b.type);
    long number = force(arguments[1])->a.number;

    if (number < 0 || number >= head->constructor_count) {
        die("toEnum at %s: %ld is out of range", head->name, number);
    }
    return make_con((int)number, 0);
}

/* derivedFromEnum :: a -> Int */
static Value *derived_fromEnum(Value **arguments)
{
    return make_int(force(arguments[1])->tag);
}

/* The constructors from `first` in steps of `step`, as a list. */
static Value *enumeration(const TypeCon *head, int first, int step)
{
    Value *list = make_con(TAG_NIL, 0);
    Value **last = &list;
    int tag;

    for (tag = first; tag >= 0 && tag < head->constructor_count; tag += step) {
        Value *cell = make_con(TAG_CONS, 2);

        cell->b.fields[0] = make_con(tag, 0);
        cell->b.fields[1] = *last;
        *last = cell;
        last = &cell->b.fields[1];
    }
    return list;
}

/* derivedEnumFrom :: a -> [a] */
static Value *derived_enumFrom(Value **arguments)
{
    return enumeration(type_head(force(arguments[0])->b.type),
                       force(arguments[1])->tag, 1);
}

/* derivedEnumFromThen :: a -> a -> [a] */
static Value *derived_enumFromThen(Value **arguments)
{
    int first = force(arguments[1])->tag;
    int second = force(arguments[2])->tag;

    if (first == second) {
        TODO("enumFromThen with the same value twice");
    }
    return enumeration(type_head(force(arguments[0])->b.type), first,
                       second - first);
}

/* minBound or maxBound: the first or last constructor of an enumeration,
   or the one constructor with the bound of each field. */
static Value *derived_bound(Type *type, const char *method)
{
    TypeCon *head = type_head(type);
    int is_min = strcmp(method, "minBound") == 0;
    Constructor *constructor;
    Value *value;
    int i;

    if (head->constructor_count == 0) {
        die("%s at %s, which has no constructors", method, head->name);
    }
    constructor = head->constructors[is_min ? 0 : head->constructor_count - 1];
    if (constructor->is_newtype) {
        return find_method(prelude_method(method),
                           field_type(type, constructor, 0));
    }
    value = make_con(constructor->tag, constructor->arity);
    for (i = 0; i < constructor->arity; i++) {
        value->b.fields[i] = find_method(prelude_method(method),
                                       field_type(type, constructor, i));
    }
    return value;
}

/* ---- Semigroup and Monoid, field by field ----

   deriving (Semigroup, Monoid) via Generically T, for a T with one
   constructor: two values are put together by putting their fields
   together, and the empty value has the empty value of each field. This
   is the only `deriving via` there is. */

/* derivedAppend :: a -> a -> a
   Upstream puts two fields together when the result is looked at; here it
   is done at once. */
static Value *derived_append(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;
    Value *a = arguments[1];
    Value *b = arguments[2];
    Value **a_fields;
    Value **b_fields;
    Constructor *constructor = constructor_of(type, &a, &a_fields);
    Value *value;
    int i;

    constructor_of(type, &b, &b_fields);
    value = make_con(constructor->tag, constructor->arity);
    for (i = 0; i < constructor->arity; i++) {
        Value *append =
            find_method(prelude_method("<>"), field_type(type, constructor, i));

        value->b.fields[i] = apply(apply(append, a_fields[i]), b_fields[i]);
    }
    return value;
}

static Value *derived_mempty(Type *type)
{
    Constructor *constructor = type_head(type)->constructors[0];
    Value *value = make_con(constructor->tag, constructor->arity);
    int i;

    for (i = 0; i < constructor->arity; i++) {
        value->b.fields[i] = find_method(prelude_method("mempty"),
                                       field_type(type, constructor, i));
    }
    return value;
}

/* ---- NFData ---- */

/* deepForce :: a -> ()
   Evaluates the whole value. The last field is done in a loop, not a
   nested call, so that a long list does not use the C stack.

   A value that has been evaluated like this says so (is_deep), and is not
   walked again. A value is often a part of many others: the name of a
   file is in every position of a syntax tree. */
Value *builtin_deepForce(Value **arguments)
{
    Value *value = arguments[1];
    int i;

    for (;;) {
        value = force(value);
        if (value->kind != V_CON || value->a.field_count == 0
            || value->is_deep) {
            break;
        }
        for (i = 0; i < value->a.field_count - 1; i++) {
            Value *field_arguments[2];

            field_arguments[0] = NULL;
            field_arguments[1] = value->b.fields[i];
            builtin_deepForce(field_arguments);
        }
        value = value->b.fields[value->a.field_count - 1];
    }
    /* All of it is evaluated now, so every value along the last fields can
       say so. */
    value = arguments[1];
    for (;;) {
        value = force(value);
        if (value->kind != V_CON || value->a.field_count == 0
            || value->is_deep) {
            break;
        }
        value->is_deep = 1;
        value = value->b.fields[value->a.field_count - 1];
    }
    return make_con(TAG_UNIT, 0);
}

/* ---- Data ---- */

/* dataFields :: a -> [Field]
   The fields of the value, each with its type: Field is
   `forall d. Field d`, so it keeps the type in front of the field. The
   derived gmapT, gmapQ and gmapM are written with it, in
   lib/Data/Data.hs. */
Value *builtin_dataFields(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;
    Value *value = arguments[1];
    Value **fields;
    Constructor *constructor = constructor_of(type, &value, &fields);
    Value *list = make_con(TAG_NIL, 0);
    int i;

    for (i = constructor->arity - 1; i >= 0; i--) {
        Value *field = make_con(0, 2);
        Value *cell = make_con(TAG_CONS, 2);

        field->b.fields[0] = make_type(field_type(type, constructor, i));
        field->b.fields[1] = fields[i];
        cell->b.fields[0] = field;
        cell->b.fields[1] = list;
        list = cell;
    }
    return list;
}

/* dataRebuild :: a -> [Field] -> a
   The value with its fields replaced by those of the list. */
Value *builtin_dataRebuild(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;
    Value *value = arguments[1];
    Value *list = force(arguments[2]);
    Value **fields;
    Constructor *constructor = constructor_of(type, &value, &fields);
    Value *result;
    int i;

    if (constructor->is_newtype) {
        return force(force(list->b.fields[0])->b.fields[1]);
    }
    result = make_con(value->tag, constructor->arity);
    for (i = 0; i < constructor->arity; i++) {
        result->b.fields[i] = force(list->b.fields[0])->b.fields[1];
        list = force(list->b.fields[1]);
    }
    return result;
}

/* The function of lib/Data/Data.hs that is a derived method of Data, at
   the type. */
static Value *data_method(const char *name, Type *type)
{
    Global *global = library_entity("Data.Data", NS_VARIABLE, name)->global;

    return apply(global_value(global), make_type(type));
}

/* ---- Functor, Foldable and Traversable ----

   The class is of a type with its last parameter left off: Functor T for
   data T b a. The methods work at the full type, with a marker in place of
   that parameter, and go through each field by its type:

   - the marker itself: the function is applied to the field;
   - a type without the marker: the field stays as it is (fmap), is
     skipped (foldr), or is given back with `pure` (traverse);
   - a tuple: each of its fields, the same way;
   - any other type with the marker in its last argument only, like [a] or
     Maybe (b, a): the method of that type's own instance, with a function
     that does this for the argument.

   That is what GHC derives, with fmap and the fields of foldr done at once
   rather than when they are looked at. The instance methods take their
   own type variables after the class's, in the order the signature names
   them, context first, so these builtins do too: fmap @a @b, foldr @a @b,
   traverse @f @a @b. */

static Binding *marker_binding = NULL;

static Type *marker(void)
{
    static Type *type = NULL;

    if (type == NULL) {
        marker_binding = xmalloc(sizeof(Binding));
        memset(marker_binding, 0, sizeof(Binding));
        marker_binding->name = "<the last parameter>";
        type = new_rigid(marker_binding);
    }
    return type;
}

static int is_marker(Type *type)
{
    type = prune(type);
    return type->kind == TY_RIGID && type->variable == marker_binding
        && marker_binding != NULL;
}

static int mentions_marker(Type *type)
{
    type = prune(type);
    if (type->kind == TY_APP) {
        return mentions_marker(type->function)
            || mentions_marker(type->argument);
    }
    return is_marker(type);
}

/* The type with the marker replaced. */
static Type *instead_of_marker(Type *type, Type *replacement)
{
    Binding *variables[1];
    Type *types[1];

    variables[0] = marker_binding;
    types[0] = replacement;
    return substitute(type, 1, variables, types);
}

/* Whether a value of the type is gone through field by field: the type the
   instance is for, and tuples. */
static int is_structural(Type *type, const TypeCon *derived)
{
    TypeCon *head = type_head(type);

    return head == derived
        || (head->parameter_count >= 2
            && head == tuple_type_constructor(head->parameter_count));
}

/* The type of an application, without its last argument; dies if the
   marker is anywhere but there. */
static Type *functor_part(Type *type)
{
    type = prune(type);
    if (type->kind != TY_APP || mentions_marker(type->function)) {
        TODO("deriving Functor, Foldable or Traversable over a field whose "
             "type has the last parameter elsewhere than in its last argument");
    }
    return type->function;
}

static Value *with_types(Value *function, Type *a, Type *b)
{
    return apply(apply(function, make_type(a)), make_type(b));
}

static Value *map_at(Type *type, const TypeCon *derived, Type *a, Type *b,
                     Value *f, Value *value);
static const Builtin map_at_builtin;

/* derivedFmapAt :: type -> a -> b -> (a -> b) -> x -> y, for a field of
   the type `type`. */
static Value *derived_fmap_at(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;

    return map_at(type, type_head(type), force(arguments[1])->b.type,
                  force(arguments[2])->b.type, arguments[3], arguments[4]);
}

static const Builtin map_at_builtin = { "derivedFmapAt", 5, derived_fmap_at };

static Value *map_at(Type *type, const TypeCon *derived, Type *a, Type *b,
                     Value *f, Value *value)
{
    if (is_marker(type)) {
        return apply(f, value);
    }
    if (!mentions_marker(type)) {
        return value;
    }
    if (is_structural(type, derived)) {
        Value **fields;
        Constructor *constructor = constructor_of(type, &value, &fields);
        Value *result;
        int i;

        if (constructor->is_newtype) {
            return map_at(field_type(type, constructor, 0), derived, a, b, f,
                          value);
        }
        result = make_con(constructor->tag, constructor->arity);
        for (i = 0; i < constructor->arity; i++) {
            result->b.fields[i] = map_at(field_type(type, constructor, i),
                                       derived, a, b, f, fields[i]);
        }
        return result;
    } else {
        Type *inner = prune(type)->argument;
        Value *mapper = with_types(apply(make_builtin(&map_at_builtin),
                                         make_type(inner)),
                                   a, b);
        Value *fmap = find_method(prelude_method("fmap"), functor_part(type));

        fmap = with_types(fmap, instead_of_marker(inner, a),
                          instead_of_marker(inner, b));
        return apply(apply(fmap, apply(mapper, f)), value);
    }
}

/* derivedFmap :: t -> a -> b -> (a -> b) -> t a -> t b */
static Value *derived_fmap(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;

    return map_at(new_type_application(type, marker()), type_head(type),
                  force(arguments[1])->b.type, force(arguments[2])->b.type,
                  arguments[3], arguments[4]);
}

static Value *fold_at(Type *type, const TypeCon *derived, Type *a, Type *b,
                      Value *f, Value *end, Value *value);

/* derivedFoldrAt :: type -> a -> b -> (a -> b -> b) -> x -> b -> b */
static Value *derived_foldr_at(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;

    return fold_at(type, type_head(type), force(arguments[1])->b.type,
                   force(arguments[2])->b.type, arguments[3], arguments[5],
                   arguments[4]);
}

static const Builtin foldr_at_builtin =
    { "derivedFoldrAt", 6, derived_foldr_at };

static Value *fold_at(Type *type, const TypeCon *derived, Type *a, Type *b,
                      Value *f, Value *end, Value *value)
{
    if (is_marker(type)) {
        return apply(apply(f, value), end);
    }
    if (!mentions_marker(type)) {
        return end;
    }
    if (is_structural(type, derived)) {
        Value **fields;
        Constructor *constructor = constructor_of(type, &value, &fields);
        int i;

        for (i = constructor->arity - 1; i >= 0; i--) {
            end = fold_at(field_type(type, constructor, i), derived, a, b, f,
                          end, fields[i]);
        }
        return end;
    } else {
        Type *inner = prune(type)->argument;
        Value *folder = with_types(apply(make_builtin(&foldr_at_builtin),
                                         make_type(inner)),
                                   a, b);
        Value *foldr = find_method(prelude_method("foldr"), functor_part(type));

        foldr = with_types(foldr, instead_of_marker(inner, a), b);
        return apply(apply(apply(foldr, apply(folder, f)), end), value);
    }
}

/* derivedFoldr :: t -> a -> b -> (a -> b -> b) -> b -> t a -> b */
static Value *derived_foldr(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;

    return fold_at(new_type_application(type, marker()), type_head(type),
                   force(arguments[1])->b.type, force(arguments[2])->b.type,
                   arguments[3], arguments[4], arguments[5]);
}

static Value *traverse_at(Type *type, const TypeCon *derived, Type *a,
                          Type *applicative, Type *b, Value *f, Value *value);

/* derivedTraverseAt :: type -> f -> a -> b -> (a -> f b) -> x -> f y */
static Value *derived_traverse_at(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;

    return traverse_at(type, type_head(type), force(arguments[2])->b.type,
                       force(arguments[1])->b.type, force(arguments[3])->b.type,
                       arguments[4], arguments[5]);
}

static const Builtin traverse_at_builtin =
    { "derivedTraverseAt", 6, derived_traverse_at };

/* pure @f @type value */
static Value *pure_at(Type *applicative, Type *type, Value *value)
{
    Value *pure = find_method(prelude_method("pure"), applicative);

    return apply(apply(pure, make_type(type)), value);
}

static Value *traverse_at(Type *type, const TypeCon *derived, Type *a,
                          Type *applicative, Type *b, Value *f, Value *value)
{
    if (is_marker(type)) {
        return apply(f, value);
    }
    if (!mentions_marker(type)) {
        return pure_at(applicative, type, value);
    }
    if (is_structural(type, derived)) {
        Value **fields;
        Constructor *constructor = constructor_of(type, &value, &fields);
        Type **types;
        Type *function;
        Value *result;
        int i;

        if (constructor->is_newtype) {
            return traverse_at(field_type(type, constructor, 0), derived, a,
                               applicative, b, f, value);
        }
        /* pure C <*> field 0 <*> field 1 ..., with the type of what is still
           a function at each step. */
        types = gc_alloc((size_t)constructor->arity * sizeof(Type *) + 1);
        function = instead_of_marker(type, b);
        for (i = constructor->arity - 1; i >= 0; i--) {
            types[i] = instead_of_marker(field_type(type, constructor, i), b);
            function = function_type(types[i], function);
        }
        result = pure_at(applicative, function, make_constructor(constructor));
        for (i = 0; i < constructor->arity; i++) {
            Value *ap = find_method(prelude_method("<*>"), applicative);
            Type *rest;

            split_function_type(function, &types[i], &rest);
            ap = with_types(ap, types[i], rest);
            result = apply(apply(ap, result),
                           traverse_at(field_type(type, constructor, i),
                                       derived, a, applicative, b, f,
                                       fields[i]));
            function = rest;
        }
        return result;
    } else {
        Type *inner = prune(type)->argument;
        Value *traverser = apply(
            with_types(apply(make_builtin(&traverse_at_builtin),
                             make_type(inner)),
                       applicative, a),
            make_type(b));
        Value *traverse =
            find_method(prelude_method("traverse"), functor_part(type));

        traverse = with_types(apply(traverse, make_type(applicative)),
                              instead_of_marker(inner, a),
                              instead_of_marker(inner, b));
        return apply(apply(traverse, apply(traverser, f)), value);
    }
}

/* derivedTraverse :: t -> f -> a -> b -> (a -> f b) -> t a -> f (t b) */
static Value *derived_traverse(Value **arguments)
{
    Type *type = force(arguments[0])->b.type;

    return traverse_at(new_type_application(type, marker()), type_head(type),
                       force(arguments[2])->b.type, force(arguments[1])->b.type,
                       force(arguments[3])->b.type, arguments[4], arguments[5]);
}

/* ---- Choosing the method ---- */

static const Builtin eq_builtin = { "derivedEq", 3, derived_eq };
static const Builtin compare_builtin = { "derivedCompare", 3, derived_compare };
static const Builtin showsPrec_builtin =
    { "derivedShowsPrec", 4, derived_showsPrec };
static const Builtin append_builtin = { "derivedAppend", 3, derived_append };
static const Builtin fmap_builtin = { "derivedFmap", 5, derived_fmap };
static const Builtin foldr_builtin = { "derivedFoldr", 6, derived_foldr };
static const Builtin traverse_builtin =
    { "derivedTraverse", 6, derived_traverse };
static const Builtin toEnum_builtin = { "derivedToEnum", 2, derived_toEnum };
static const Builtin fromEnum_builtin =
    { "derivedFromEnum", 2, derived_fromEnum };
static const Builtin enumFrom_builtin =
    { "derivedEnumFrom", 2, derived_enumFrom };
static const Builtin enumFromThen_builtin =
    { "derivedEnumFromThen", 3, derived_enumFromThen };

/* The classes GHC can derive by looking at the constructors. */
static int is_stock_class(const char *name)
{
    static const char *names[] = {
        "Eq", "Ord", "Show", "Read", "Enum", "Bounded", "Ix", "Functor",
        "Foldable", "Traversable", "Generic", "Data", "Typeable", "Lift", NULL
    };
    int i;

    for (i = 0; names[i] != NULL; i++) {
        if (strcmp(name, names[i]) == 0) {
            return 1;
        }
    }
    return 0;
}

Value *derived_method(const Instance *instance, Global *method, Type *type)
{
    const TypeCon *head = instance->derived_type;
    const char *class_name = method->class->name;
    const char *name = method->name;
    int strategy = instance->derived_strategy;

    if (strategy == DERIVE_DEFAULT) {
        /* What GHC does when the clause names no strategy. */
        if (is_stock_class(class_name)) {
            strategy = DERIVE_STOCK;
        } else if (head->constructor_count == 1
                   && head->constructors[0]->is_newtype) {
            strategy = DERIVE_NEWTYPE;
        } else {
            strategy = DERIVE_ANYCLASS;
        }
    }
    switch (strategy) {
    case DERIVE_VIA:
        if (head->constructor_count != 1 || head->constructors[0]->is_newtype) {
            TODO("deriving via for a type that is not one constructor with "
                 "fields");
        }
        if (strcmp(class_name, "Semigroup") == 0 && strcmp(name, "<>") == 0) {
            return at_type(&append_builtin, type);
        }
        if (strcmp(class_name, "Monoid") == 0) {
            /* The other methods are the class's defaults. */
            return strcmp(name, "mempty") == 0 ? derived_mempty(type) : NULL;
        }
        TODO("deriving via for a class other than Semigroup and Monoid");
        break;
    case DERIVE_NEWTYPE:
        /* The instance of the type inside the newtype. */
        return find_method(method, field_type(type, head->constructors[0], 0));
    case DERIVE_ANYCLASS:
        return NULL; /* every method is the class's default */
    default:
        break;
    }
    if (strcmp(class_name, "Eq") == 0 && strcmp(name, "==") == 0) {
        return at_type(&eq_builtin, type);
    }
    if (strcmp(class_name, "Ord") == 0 && strcmp(name, "compare") == 0) {
        return at_type(&compare_builtin, type);
    }
    if (strcmp(class_name, "Show") == 0 && strcmp(name, "showsPrec") == 0) {
        return at_type(&showsPrec_builtin, type);
    }
    if (strcmp(class_name, "Enum") == 0) {
        if (strcmp(name, "toEnum") == 0) {
            return at_type(&toEnum_builtin, type);
        }
        if (strcmp(name, "fromEnum") == 0) {
            return at_type(&fromEnum_builtin, type);
        }
        if (strcmp(name, "enumFrom") == 0) {
            return at_type(&enumFrom_builtin, type);
        }
        if (strcmp(name, "enumFromThen") == 0) {
            return at_type(&enumFromThen_builtin, type);
        }
    }
    if (strcmp(class_name, "Bounded") == 0) {
        return derived_bound(type, name);
    }
    if (strcmp(class_name, "Data") == 0) {
        if (strcmp(name, "gmapT") == 0) {
            return data_method("derivedGmapT", type);
        }
        if (strcmp(name, "gmapQ") == 0) {
            return data_method("derivedGmapQ", type);
        }
        if (strcmp(name, "gmapM") == 0) {
            return data_method("derivedGmapM", type);
        }
        TODO("derived method of Data other than gmapT, gmapQ and gmapM");
    }
    /* The other methods of these classes are the defaults. */
    if (strcmp(class_name, "Functor") == 0 && strcmp(name, "fmap") == 0) {
        return at_type(&fmap_builtin, type);
    }
    if (strcmp(class_name, "Foldable") == 0 && strcmp(name, "foldr") == 0) {
        return at_type(&foldr_builtin, type);
    }
    if (strcmp(class_name, "Traversable") == 0
        && strcmp(name, "traverse") == 0) {
        return at_type(&traverse_builtin, type);
    }
    if (strcmp(class_name, "Read") == 0) {
        TODO("derived instance of Read");
    }
    return NULL;
}
