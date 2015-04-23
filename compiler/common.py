from collections import defaultdict

LANGUAGE = "cpp"

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
}


def c_repr(string):
    return '"{0}"'.format(repr(string)[1:-1].replace('"', r'\"'))


def null_constant():
    return null_constants[LANGUAGE]


def is_keyword(string):
    return string.lower() in keywords[LANGUAGE]


__next_ids = defaultdict(int)


def gen_name(var_name):
    __next_ids[var_name] += 1
    return "{0}_{1}".format(var_name, __next_ids[var_name])


def gen_names(*var_names):
    for var_name in var_names:
        yield gen_name(var_name)


basic_types = {
    "ada": {
        long: "Long_Integer",
        bool: "Boolean"
    },
    "cpp": {
        long: "long",
        bool: "bool"
    }
}


def get_type(typ):
    return basic_types[LANGUAGE][typ]
