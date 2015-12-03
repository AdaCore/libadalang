"""
This file contains general utility functions. It is basically the support
library for langkit, aggregating general python utilities that we could not
find in the standard library.
"""
from copy import copy
from itertools import takewhile


class StructEq(object):
    """
    Mixin for structural equality.
    """
    def __eq__(self, other):
        if type(other) is type(self):
            if hasattr(self, "_eq_keys"):
                eq_keys = self._eq_keys
            elif hasattr(self, "_excl_eq_keys"):
                eq_keys = set(self.__dict__.keys()) ^ set(self._excl_eq_keys)
            else:
                return self.__dict__ == other.__dict__

            return all(v == other.__dict__[k] for k, v in self.__dict__
                       if k in eq_keys)

        return False


def unescape(char):
    """
    Unescape char if it is escaped.

    :param str char: An eventually escaped character
    :rtype: str
    """
    if char[0] == "\\":
        return char[1:]
    return char


def copy_with(obj, **kwargs):
    """
    Copy an object, and add every key value association passed in kwargs to it.

    :type obj: T
    :rtype: T
    """
    c = copy(obj)

    for k, v in kwargs.items():
        setattr(c, k, v)

    return c


class Colors:
    """
    Utility escape sequences to color output in terminal.
    """
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'


def col(msg, color):
    return "{0}{1}{2}".format(color, msg, Colors.ENDC)


def printcol(msg, color):
    """
    Utility print function that will print `msg` in color `color`.
    :param basestring msg: The message to print.
    :param basestring color: The color escape sequence from the enum class
        Colors which represents the color to use.
    :return: The color-escaped string, resetting the color to blank at the end.
    :rtype: basestring
    """
    print col(msg, color)


def common_ancestor(*classes):
    """
    Return the bottom-most common parent class for all `classes`.

    :param classes: The classes for which we are searching a common ancestor.
    :type classes: list[types.ClassType]
    :rtype: types.ClassType
    """
    rmro = lambda k: reversed(k.mro())
    result = list(takewhile(lambda a: len(set(a)) == 1,
                            zip(*map(rmro, classes))))[-1][0]
    return result


def memoized(func):
    """
    Decorator to memoize a function.
    This function must be passed only hashable arguments.
    """
    cache = {}

    def wrapper(*args, **kwargs):
        key = (args, tuple(kwargs.items()))
        try:
            result = cache[key]
        except KeyError:
            result = func(*args, **kwargs)
            cache[key] = result
            result = result
        return result

    return wrapper


def type_check(klass):
    """
    Return a predicate that will return true if its parameter is a subclass
    of `klass`.
    :param type klass: Class to check against.
    :rtype: (T) -> bool
    """
    return lambda t: t and issubclass(t, klass)


def type_check_instance(klass):
    """
    Return a predicate that will return true if its parameter is a subclass
    of `klass`.
    :param type klass: Class to check against.
    :rtype: (T) -> bool
    """
    return lambda t: isinstance(t, klass)
