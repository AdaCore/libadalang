from collections import defaultdict
import itertools

import names

LANGUAGE = "ada"

TOKEN_PREFIX = "QUEX_TKN_"

languages_extensions = {
    "ada": "adb",
    "cpp": "cpp",
}

null_constants = {
    "ada": "null",
    "cpp": "nullptr"
}

keywords = {
    "cpp": set("""
        align as alignof and and_eq asm auto
        bitand bitor bool break
        case catch char char16_t char32_t class compl concept const constexpr
        const_cast continue
        decltype default delete do double dynamic_cast
        else enum explicit export extern
        false float for friend
        goto
        if inline int
        long
        mutable
        namespace new noexcept not not_eq nullptr
        operator or or_eq
        private protected public
        register reinterpret_cast requires return
        short signed sizeof static static_assert static_cast struct switch
        template this thread_local throw true try typedef typeid typename
        union unsigned using
        virtual void volatile
        wchar_t while
        xor xor_eq
    """.split()),

    "ada": set("""
    abort abs abstract accept access aliased all and array at
    begin body
    case constant
    declare delay delta digits do
    else elsif end entry exception exit
    for function
    generic goto
    if in interface is
    limited loop
    mod
    new not null
    of or others out overriding
    package pragma private procedure protected
    raise range record rem renames requeue
    return reverse
    select separate some subtype synchronized
    tagged task terminate then type
    until use
    when while with xor
    """.split()),
}


def c_repr(string):
    return '"{0}"'.format(repr(string)[1:-1].replace('"', r'\"'))


def null_constant():
    return null_constants[LANGUAGE]


def is_keyword(string):
    return string.lower() in keywords[LANGUAGE]


__next_ids = defaultdict(lambda: itertools.count(0))


def gen_name(var_name):
    # This function is mostly used to generate temporary variables in parsing
    # functions. In these places it's more convenient to give lower case names
    # as it melts fine with the rest of the Python code.
    if isinstance(var_name, basestring):
        var_name = names.Name.from_lower(var_name)

    var_id = next(__next_ids[var_name.lower])
    return var_name + names.Name(str(var_id))


def gen_names(*var_names):
    for var_name in var_names:
        yield gen_name(var_name)


basic_types = {
    "ada": {
        long: "Integer",
        bool: "Boolean"
    },
    "cpp": {
        long: "long",
        bool: "bool"
    }
}


def get_type(typ):
    return basic_types[LANGUAGE][typ]
