from copy import copy


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
    if char[0] == "\\":
        return char[1:]
    return char


def copy_with(obj, **kwargs):
    """
    :type obj: T
    :rtype: T
    """
    c = copy(obj)

    for k, v in kwargs.items():
        setattr(c, k, v)

    return c


def make_tupled(fn):

    def _call(tpl):
        return fn(*tpl)

    return _call


def extract(lst, idx=0, alt=None):
    if lst:
        return lst[idx]
    else:
        return alt


def isalambda(v):
    return isinstance(v, type(lambda: None)) and v.__name__ == '<lambda>'


class Colors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'


def printcol(msg, color):
    print "{0}{1}{2}".format(color, msg, Colors.ENDC)

