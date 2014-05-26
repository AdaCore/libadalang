from combinators import *
from tokenizer import *
import unittest


class CombinatorsTest(unittest.TestCase):
    def test_tok(self):
        tok_rule = Tok(Punc("+"))
        assert tok_rule.test_parser("+").val == "+"

    def test_tok_class(self):
        tok_rule = TokClass(Id)
        assert tok_rule.test_parser("hello").val == "hello"

    def test_or(self):
        or_rule = Or(Punc("+"), Kw("function"))
        assert or_rule.test_parser("+").val == "+"
        assert or_rule.test_parser("function").val == "function"

    def test_row(self):
        row_rule = Row(Punc("+"), Kw("function"))
        self.assertEqual(str(row_rule.test_parser("+ function")),
                         "[Punc('+'), Kw('function')]")

    def test_or_row(self):
        or_row_rule = Row(Or(Punc("+"), Punc("-")),
                          Or(Punc("*"), Punc("/")))
        self.assertEqual(str(or_row_rule.test_parser("+ /")),
                         "[Punc('+'), Punc('/')]")

    def test_list(self):
        list_rule = List(Id, sep=Punc("+"))
        self.assertEqual(str(list_rule.test_parser("A + B + C + D")),
                         "[Id('A'), Id('B'), Id('C'), Id('D')]")

    def test_composite(self):
        rule = List(Row(Or(Punc("+"), Punc("-")),
                        Or(Punc("*"), Punc("/"))), sep=Punc(","))
        res = [[Punc('+'), Punc('/')], [Punc('-'), Punc('*')],
               [Punc('-'), Punc('*')]]
        self.assertEqual(str(rule.test_parser("+ /, - *, - *")),
                         str(res))

    def test_list_empty(self):
        rule = Row(Punc("("), List(Id, sep=Punc(","), empty_valid=True),
                   Punc(")"))

        res1 = [Punc('('), [Id('A'), Id('B'), Id('C'), Id('D')], Punc(')')]
        res2 = [Punc('('), [], Punc(')')]
        self.assertEqual(str(rule.test_parser("(A, B, C, D)")), str(res1))
        self.assertEqual(str(rule.test_parser("()")), str(res2))

        assert not Row(Punc("("), List(Id, sep=Punc(",")),
                       Punc(")")).test_parser("()")

    def test_opt(self):
        rule = List(Row(Opt(Kw("begin")), Id), sep=Punc(","))
        res = [[Kw('begin'), Id('B')], [None, Id('C')], [None, Id('D')],
               [Kw('begin'), Id('F')], [None, Id('G')]]
        self.assertEqual(str(rule.test_parser("begin B, C, D, begin F, G")),
                         str(res))

    def test_discard(self):
        rule = Row(_(Id), Punc(","), Id)
        res = [Punc(','), Id('B')]
        self.assertEqual(str(rule.test_parser("A, B")), str(res))

    def test_success(self):
        rule = Row(Id, Punc(","), Success(lambda: 42), Id)
        print rule.test_parser("A, B")

    def test_defer(self):
        rule = Row(Defer(lambda: rule_def), Punc(","), Id)
        rule_def = List(Id, sep=Punc(";"))

        print rule.test_parser("A;B;C;D,E")

    def test_transform(self):
        rule = Row(Id, Punc(","), Id) ^ (lambda (i, _, j): i.val + j.val)
        self.assertEqual(rule.test_parser("A, B"), "AB")
