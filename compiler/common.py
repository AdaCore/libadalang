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


def c_repr(string):
    return '"{0}"'.format(repr(string)[1:-1].replace('"', r'\"'))


def null_constant():
    return null_constants[LANGUAGE]


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
