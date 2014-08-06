import re

from utils import copy_with


class Token(object):

    is_ptr = False

    @classmethod
    def nullexpr(cls):
        return "no_token"

    @classmethod
    def as_string(cls):
        return cls.__name__

    _match_attrs = ["val"]

    def __init__(self, val, line=-1, col=-1):
        self.val = val
        self.line = line
        self.col = col

    def does_match(self, other):
        return type(self) == type(other) and self.val == other.val

    def __repr__(self):
        return "{0}({1})".format(
            self.__class__.__name__,
            repr(self.val),
            self.line,
            self.col
        )

    def __or__(self, other):
        from combinators import Or
        assert isinstance(other, Token)
        return Or(self, other)

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.val == other.val


class NumLit(Token):

    def __init__(self, val, line=-1, column=-1):
        Token.__init__(self, val, line, column)


class StringLit(Token):

    def __init__(self, val, line=-1, column=-1):
        Token.__init__(self, val, line, column)


class Kw(Token):

    ada_keywords = set((
        "abort else new return abs elsif not reverse abstract end null accept "
        "entry select access exception of separate aliased exit or some all "
        "others subtype and for out synchronized array function overriding "
        "at tagged generic package task begin goto pragma terminate body "
        "private then if procedure type case in protected constant "
        "interface is raise use declare range delay limited record when "
        "delta loop rem while digits renames with do mod requeue xor"
    ).split(" "))
    regex = r"\b(?:%s)\b" % "|".join(ada_keywords)

    def __init__(self, kw, line=-1, column=-1):
        """
        :type kw: string
        """
        assert kw.lower() in self.ada_keywords
        Token.__init__(self, kw, line, column)


class Lbl(Token):
    pass


class Id(Token):


    def __init__(self, idn, line=-1, column=-1):
        """
        :type idn: string
        """
        Token.__init__(self, idn, line, column)


class Punc(Token):
    punctuations = (r"(", r")", ";", ":", ",", r"..", r".", r"<>", r"<=",
                    r">=", r"=>", r"=", r"<", r">", r"+", r"-", r'**', r'*',
                    "&", "/=", "/", r"'", r"|")

    escaped_puncs = [re.escape(p) for p in punctuations]

    regex = r"(?:%s)" % "|".join(escaped_puncs)

    def __init__(self, punc, line=-1, column=-1):
        assert punc in self.punctuations
        Token.__init__(self, punc, line, column)


class CharLit(Token):
    regex = r"'.'"


class Comment(Token):
    regex = r'--.*?$'


class NoToken(Token):
    def __init__(self):
        Token.__init__(self, None, -1, -1)


def get_matching_group_idx(m):
    i = 0
    for g in m.groups():
        if g:
            return i
        i += 1


def tokenize(string, patterns_to_fns):
    """
    :type string: string
    :type patterns_to_fns: dict[string, (string, int, int, int) -> Token]
    """
    patterns, fns = zip(*patterns_to_fns)
    re_str = "|".join("(%s)" % p for p in patterns)
    regexp = re.compile(re_str, re.M | re.I)
    current_offset = 0
    current_line = 0
    res = []

    while current_offset != -1:
        next_offset = string.find("\n", current_offset + 1)
        end_match = next_offset if next_offset != -1 else len(string)

        for m in regexp.finditer(string, current_offset, end_match):
            idx = get_matching_group_idx(m)
            offset = m.start(idx + 1)

            if not fns[idx]:
                continue

            res.append(fns[idx](m.group(), current_line, offset -
                                current_offset))

        current_offset = next_offset

    return res

no_token = NoToken()

class Tokenizer(object):

    def __init__(self, patterns_to_fns, string):
        self.tokens = tokenize(string, patterns_to_fns)
        self.nb_tokens = len(self.tokens)

    def get(self, offset):
        """
        :rtype: Token
        """
        if offset >= self.nb_tokens:
            return no_token
        else:
            return self.tokens[offset]

    def match(self, pos, *tokens):
        return all(t.does_match(self.get(pos + o)) if isinstance(t, Token)
                   else isinstance(self.get(o), t)
                   for o, t in enumerate(tokens))


def make_ada_tokenizer(string):
    return Tokenizer((
        # Comments
        (Comment.regex, None),

        # Keywords
        (Kw.regex, Kw),

        # Identifiers
        (r"[a-zA-Z_][a-zA-Z\d_]*", Id),

        # Character literals
        (CharLit.regex, CharLit),

        # Punctuation
        (Punc.regex, Punc),

        # Strings
        (r'".*?"', StringLit),

        # Based numbers
        (r'\d+#[\dA-F_]+(?:\.[\dA-F_]+)?(?:E[+-]?[\d_]+)?', NumLit),

        # Literal numbers
        (r"\d[\d_]*(?:\.\d[\d_]*)?(?:E[+-]?[\d_]+)?", NumLit),

    ), string)


