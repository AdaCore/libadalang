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

    def __init__(self, val=None):
        self.val = val

    def __repr__(self):
        return "{0}({1})".format(
            self.__class__.__name__,
            repr(self.val),
        )

    def __or__(self, other):
        from combinators import Or
        assert isinstance(other, Token)
        return Or(self, other)

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.val == other.val


class NumLit(Token):
    pass


class StringLit(Token):
    pass


class Kw(Token):
    pass


class Lbl(Token):
    pass


class Id(Token):
    pass


class Punc(Token):
    pass


class CharLit(Token):
    pass


class Comment(Token):
    pass


class NoToken(Token):
    pass


no_token = NoToken()
