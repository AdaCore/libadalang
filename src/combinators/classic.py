from tokenizer import Tokenizer, Token, make_ada_tokenizer
from utils import make_tupled, isalambda


NoResult = (None, None)


class ParserContainer(object):

    @classmethod
    def get_parser(cls):
        """
        :rtype Combinator
        """
        return None


def p_list_gen(tkz, pos, parser, sep=None):
    """
    :type parser: Combinator
    :type pos: int
    :type tkz: Tokenizer
    :type sep: Token
    :rtype: collections.Iterable[(int, list[T])]
    """
    current_pos = pos
    while True:
        new_pos, node = parser.parse(tkz, current_pos)
        if new_pos:
            current_pos = new_pos
            if sep:
                if tkz.get(current_pos).does_match(sep):
                    current_pos += 1
                    yield current_pos, node
                else:
                    yield new_pos, node
                    break
            else:
                yield current_pos, node
        else:
            break


def p_list(tkz, pos, parser, sep=None, empty_valid=False):
    """
    :type parser: Combinator
    :type tkz: Tokenizer
    :type sep: Token
    :rtype: (Tokenizer, list[T])
    """
    assert isinstance(tkz, Tokenizer)
    res = []
    ret_pos = None

    for c_pos, el in p_list_gen(tkz, pos, parser, sep):
        res.append(el)
        ret_pos = c_pos

    if res:
        return ret_pos, res
    elif empty_valid:
        return pos, res
    else:
        return NoResult


class Combinator(object):

    def parse(self, tkz, pos):
        raise NotImplemented

    def __or__(self, other):
        other_comb = resolve(other)
        if isinstance(other_comb, Or):
            other_comb.matchers.append(self)
            return other_comb
        elif isinstance(self, Or):
            self.matchers.append(other_comb)
            return self
        else:
            return Or(self, other_comb)

    def __xor__(self, transform_fn):
        """
        :type transform_fn: (T) => U
        :rtype: Transform
        """
        return Transform(self, transform_fn)

    def test_parser(self, ada_string):
        tkz = make_ada_tokenizer(ada_string)
        npos, res = self.parse(tkz, 0)
        return res

    def __repr__(self):
        name = getattr(self, "name", "")
        return "{0}({1})".format(self.__class__, name)


class List(Combinator):
    def __init__(self, parser, sep=None, empty_valid=False):
        """
        :type sep: Token|string|None
        """
        self.parser = resolve(parser)
        self.sep = sep

        if sep and isinstance(sep, str):
            self.sep = Token(sep)

        self.empty_valid = empty_valid

    def parse(self, tkz, pos):
        # return p_list(tkz, pos, self.parser, self.sep,
#                       empty_valid=self.empty_valid)

        current_pos = pos
        parser = self.parser
        sep = self.sep
        res = []
        while True:
            new_pos, node = parser.parse(tkz, current_pos)
            if new_pos:
                current_pos = new_pos
                res.append(node)
                if sep:
                    if tkz.get(current_pos).val == sep.val:
                        current_pos += 1
                    else:
                        break
            else:
                break

        if res:
            return current_pos, res
        elif self.empty_valid:
            return pos, res
        else:
            return NoResult


class Success(Combinator):
    def __init__(self, result_fn):
        self.result_fn = result_fn

    def parse(self, tkz, pos):
        return pos, self.result_fn()


class Defer(Combinator):
    def __init__(self, parser_fn):
        """
        :type parser_fn:
        """
        self.parser_fn = parser_fn
        self.parser = None

    def parse(self, tkz, pos):
        if not self.parser:
            self.parser = self.parser_fn()
            ":type: Combinator"
        return self.parser.parse(tkz, pos)


class Discard(Combinator):
    def __init__(self, parser):
        self.parser = resolve(parser)

    def parse(self, tkz, pos):
        return self.parser.parse(tkz, pos)


class Opt(Combinator):
    def __init__(self, matcher, *matchers):
        if matchers:
            self.matcher = Row(matcher, *matchers)
        else:
            self.matcher = resolve(matcher)

    def parse(self, tkz, pos):
        npos, res = self.matcher.parse(tkz, pos)
        return (npos, res) if npos else (pos, None)


class Row(Combinator):
    def __init__(self, *matchers):
        """
        :param matchers: list[Combinator|Token|ParserContainer]
        """
        self.matchers = [resolve(m) for m in matchers]
        self.f_matchers = [(not isinstance(m, Discard), m)
                           for m in self.matchers]

    def parse(self, tkz, pos):
        cpos = pos
        res = []

        for keep, matcher in self.f_matchers:
            cpos, cur_res = matcher.parse(tkz, cpos)

            if cpos is None:
                return NoResult

            if keep:
                res.append(cur_res)

        return cpos, tuple(res)


class Or(Combinator):

    def __init__(self, *matchers):
        """ :type matchers: list[Combinator|Token|type] """
        self.matchers = [resolve(m) for m in matchers]

    def parse(self, tkz, pos):
        """
        :type tkz: Tokenizer
        """
        for matcher in self.matchers:
            npos, res = matcher.parse(tkz, pos)
            if npos:
                return npos, res
        return NoResult


class Tok(Combinator):

    def __init__(self, tok):
        """ :type tok: Token """
        self.tok = tok

    def parse(self, tkz, pos):
        """
        :type tkz: Tokenizer
        :rtype: (Token|None, Tokenizer|None)
        """
        tk = tkz.get(pos)
        return (pos + 1, tk) if self.tok.val == tk.val else NoResult


class TokClass(Combinator):

    def __init__(self, tok_class):
        """ :type tok_class: type """
        assert issubclass(tok_class, Token)
        self.tok_class = tok_class

    def parse(self, tkz, pos):
        """
        :type tkz: Tokenizer
        :rtype: (Token|None, Tokenizer|None)
        """
        tk = tkz.get(pos)
        return (pos + 1, tk) if isinstance(tk, self.tok_class) else NoResult


class Transform(Combinator):

    def __init__(self, combinator, transform_fn):
        """
        :type combinator: Combinator
        :type transform_fn: (T) => U
        """
        self.combinator = combinator
        self.transform_fn = transform_fn
        if isinstance(self.combinator, Row):
            self.transform_fn = make_tupled(transform_fn)

    def parse(self, tkz, pos):
        npos, res = self.combinator.parse(tkz, pos)

        if npos is None:
            return NoResult

        trans_res = self.transform_fn(res)

        return npos, trans_res


def resolve(matcher):
    """
    :type matcher: Combinator|Token|ParserContainer
    :rtype: Combinator
    """
    if isinstance(matcher, Combinator):
        return matcher
    elif isinstance(matcher, type) and issubclass(matcher, Token):
        return TokClass(matcher)
    elif isinstance(matcher, Token):
        return Tok(matcher)
    elif isinstance(matcher, str):
        return Tok(Token(matcher))
    elif isalambda(matcher):
        return Defer(matcher)
    else:
        return Defer(matcher)
