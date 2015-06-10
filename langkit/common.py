from collections import defaultdict
import itertools
import names

LANGUAGE = "ada"

TOKEN_PREFIX = "QUEX_TKN_"


def get_token_kind(token_suffix):
    """
    Given a suffix `token_suffix`, returns the corresponding token kind,
    which is equal to TOKEN_PREFIX + token_suffix
    :param str|unicode token_suffix: The suffix identifiying the token kind
    :return: The full token kind name
    """
    return TOKEN_PREFIX + token_suffix

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


def string_repr(string):
    """
    Return a representation of string as a literal, usable in the generated
    code
    :param str string: The string to represent
    :return: A string literal representation of string
    """
    return '"{0}"'.format(repr(string)[1:-1].replace('"', r'""'))


def null_constant():
    """
    Return the applicable representation of the null constant given the
    chosen global language

    :return: The null constant
    :rtype: str
    """
    return null_constants[LANGUAGE]


def is_keyword(name):
    """
    Returns wether `name` is a keyword given the chosen global language

    :param str|names.Name name: The name we want to test
    :rtype: bool
    """
    if isinstance(name, names.Name):
        name = name.lower
    return name.lower() in keywords[LANGUAGE]


__next_ids = defaultdict(lambda: itertools.count(0))


def gen_name(var_name):
    """
    Generates a unique name from var_name

    :param str|names.Name var_name: The base name. If it is a string,
    it needs to be a lower case with underscores string
    :rtype: names.Name
    """
    # This function is mostly used to generate temporary variables in parsing
    # functions. In these places it's more convenient to give lower case names
    # as it melts fine with the rest of the Python code.
    if isinstance(var_name, basestring):
        var_name = names.Name.from_lower(var_name)

    var_id = next(__next_ids[var_name.lower])
    return var_name + names.Name(str(var_id))


def gen_names(*var_names):
    """
    Utility function around gen_name, meant to be used with unpacking. Will
    generate a list of names given the list of base names `var_names`. Used
    like::

        name_a, name_b = gen_names("a", "b")

    :param list[str] var_names: The list of base names
    :rtype: list[str]
    """
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
    """
    Given a base python type, long or bool, will return the representation
    of that type in the target global language
    :param long|bool typ: The type to be represented
    :rtype: str
    """
    return basic_types[LANGUAGE][typ]
