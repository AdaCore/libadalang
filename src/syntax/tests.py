from _ast import Num
from syntax import A
from syntax import decl, exprs, types
import unittest

from tokenizer import make_ada_tokenizer, Kw, Punc, Id, CharLit, NumLit
import combinators as C


class DeclsTest(unittest.TestCase):
    def test_pragmas(self):
        print str(A.pragma.test_parser("pragma Unreferenced (A, B, C)"))


class TypesTest(unittest.TestCase):
    def test_type_discr(self):
        self.assertEqual(
            str(A.type_discriminant.test_parser("(A : Integer; B : "
                                               "String)")),
            "TypeDiscriminant([DiscriminantSpec([Id(A)], TypeRef("
            "Id(Integer))), DiscriminantSpec([Id(B)], TypeRef(Id(String)))])"
        )

        self.assertEqual(
            str(A.type_discriminant.test_parser("(A : Integer; -- Lol\n"
                                              "B : String)")),
            "TypeDiscriminant([DiscriminantSpec([Id(A)], TypeRef("
            "Id(Integer))), DiscriminantSpec([Id(B)], TypeRef(Id(String)))])"
        )

        self.assertEqual(str(A.type_discriminant.test_parser("(<>)")),
                         "TypeDiscriminant(None)")

    def test_enum_type(self):
        print A.rules
        print A.rules['full_type_decl']
        self.assertEqual(
            str(A.full_type_decl.test_parser("type A is (B, C, D)")),
            "FullTypeDecl(Id(A), None, EnumTypeDef([Id(B), Id(C), Id(D)]))"
        )


class TokenizerTest(unittest.TestCase):
    def test(self):
        tokenizer = make_ada_tokenizer("procedure A is begin null; end A;")
        tokens = [
            Kw("procedure"), Id("A"), Kw("is"), Kw("begin"), Kw("null"),
            Punc(";"), Kw("end"), Id("A")
        ]

        assert tokenizer.match(0, *tokens)
        assert tokenizer.match(3, *tokens[3:])
        assert tokenizer.match(0, Kw, Id, Kw, Kw, Kw, Punc, Kw, Id)

        tokenizer_2 = make_ada_tokenizer("'a'")
        assert tokenizer_2.match(0, CharLit)

        t3 = make_ada_tokenizer("12 + 15 - 17 & 88")
        assert t3.match(0, NumLit, Punc, NumLit, Punc, NumLit, Punc, NumLit)



class CombinatorsTest(unittest.TestCase):

    def test_tok(self):
        tok_rule = C.Tok(Punc("+"))
        assert tok_rule.test_parser("+").val == "+"

    def test_tok_class(self):
        tok_rule = C.TokClass(Id)
        assert tok_rule.test_parser("hello").val == "hello"

    def test_or(self):
        or_rule = C.Or(Punc("+"), Kw("function"))
        assert or_rule.test_parser("+").val == "+"
        assert or_rule.test_parser("function").val == "function"

    def test_row(self):
        row_rule = C.Row(Punc("+"), Kw("function"))
        self.assertEqual(str(row_rule.test_parser("+ function")),
                         "(Punc('+'), Kw('function'))")

    def test_or_row(self):
        or_row_rule = C.Row(C.Or(Punc("+"), Punc("-")),
                            C.Or(Punc("*"), Punc("/")))
        self.assertEqual(str(or_row_rule.test_parser("+ /")),
                         "(Punc('+'), Punc('/'))")

    def test_list(self):
        list_rule = C.List(Id, sep=Punc("+"))
        self.assertEqual(str(list_rule.test_parser("A + B + C + D")),
                         "[Id('A'), Id('B'), Id('C'), Id('D')]")

    def test_composite(self):
        rule = C.List(C.Row(C.Or(Punc("+"), Punc("-")),
                            C.Or(Punc("*"), Punc("/"))), sep=Punc(","))
        res = [(Punc('+'), Punc('/')), (Punc('-'), Punc('*')),
               (Punc('-'), Punc('*'))]
        self.assertEqual(str(rule.test_parser("+ /, - *, - *")),
                         str(res))

    def test_list_empty(self):
        rule = C.Row(Punc("("), C.List(Id, sep=Punc(","), empty_valid=True),
                     Punc(")"))

        res1 = (Punc('('), [Id('A'), Id('B'), Id('C'), Id('D')], Punc(')'))
        res2 = (Punc('('), [], Punc(')'))
        self.assertEqual(str(rule.test_parser("(A, B, C, D)")), str(res1))
        self.assertEqual(str(rule.test_parser("()")), str(res2))

        assert not C.Row(Punc("("), C.List(Id, sep=Punc(",")),
                         Punc(")")).test_parser("()")

    def test_opt(self):
        rule = C.List(C.Row(C.Opt(Kw("begin")), Id), sep=Punc(","))
        res = [(Kw('begin'), Id('B')), (None, Id('C')), (None, Id('D')),
               (Kw('begin'), Id('F')), (None, Id('G'))]
        self.assertEqual(str(rule.test_parser("begin B, C, D, begin F, G")),
                         str(res))

    def test_discard(self):
        rule = C.Row(C._(Id), Punc(","), Id)
        res = (Punc(','), Id('B'))
        self.assertEqual(str(rule.test_parser("A, B")), str(res))

    def test_success(self):
        rule = C.Row(Id, Punc(","), C.Success(lambda: 42), Id)
        print rule.test_parser("A, B")

    def test_defer(self):
        rule = C.Row(C.Defer(lambda: rule_def), Punc(","), Id)
        rule_def = C.List(Id, sep=Punc(";"))

        print rule.test_parser("A;B;C;D,E")

    def test_transform(self):
        rule = C.Row(Id, Punc(","), Id) ^ (lambda i, _, j: i.val + j.val)
        self.assertEqual(rule.test_parser("A, B"), "AB")


class ExprTest(unittest.TestCase):

    def test_factor(self):
        self.assertEqual(
            str(A.factor.test_parser("4 ** 8")),
            "BinOp(Num(4), Punc('**'), Num(8))"
        )
        self.assertEqual(
            str(A.factor.test_parser("abs 8")),
            "UnOp(Kw('abs'), Num(8))"
        )

    def test_term(self):
        self.assertEqual(
            "BinOp(BinOp(BinOp(Num(4), Punc('*'), Num(8)), Punc('/'), UnOp(Kw("
            "'abs'), Num(5))), Kw('mod'), BinOp(Num(9), Punc('**'), Num(12)))",
            str(A.term.test_parser("4 * 8 / abs 5 mod 9 ** 12"))
        )

    def test_simple_expr(self):
        self.assertEqual(
            "BinOp(BinOp(BinOp(BinOp(BinOp(Num(4), Punc('*'), Num(8)), "
            "Punc('/')"
            ", UnOp(Kw('abs'), Num(5))), Kw('mod'), BinOp(Num(9), Punc('**'), "
            "Num(12))), Punc('+'), BinOp(Num(12), Punc('*'), Num(2))), "
            "Punc('-'), "
            "Num(7))",
            str(A.simple_expr.test_parser(
                "4 * 8 / abs 5 mod 9 ** 12 + 12 * 2 - 7"))
        )

    def test_relation(self):
        self.assertEqual(
            "BinOp(Num(12), Punc('='), BinOp(Id(a), Punc('/'), Num(2)))",
            str(A.relation.test_parser("12 = a / 2"))
        )

    def test_expression(self):
        self.assertEqual(
            "BinOp(BinOp(BinOp(Num(12), Punc('='), BinOp(Id(a), Punc('/'), "
            "Num(2))), 2, BinOp(BinOp(BinOp(Num(3), Punc('**'), Num(8)), "
            "Punc('/'), Num(2)), Punc('/='), Num(8))), 3, Id(b))",
            str(A.expression.test_parser(
                "12 = a / 2 and then 3 ** 8 / 2 /= 8 or else b"))
        )

    def test_range(self):
        self.assertEqual(
            str(A.range_expression.test_parser("12 .. 15")),
            "BinOp(Num(12), Punc('..'), Num(15))"
        )

        self.assertEqual(str(A.range_expression.test_parser("A'Range")),
                         "AttributeExpr(Id(A), Attr(Range), None)")

        self.assertEqual(
            str(A.range_expression.test_parser("A'Range(12, 16)")),
            "AttributeExpr(Id(A), Attr(Range), [Num(12), Num(16)])"
        )

        self.assertEqual(
            str(A.expression.test_parser("A not in B'Range(12)")),
            "BinOp(Id(A), 6, MembershipExpr([AttributeExpr(Id(B), "
            "Attr(Range), [Num(12)])]))"
        )

    def test_aggregates(self):
        A.aggregate.dump_to_file("aggregates2.py")
        self.assertEqual(
            str(A.aggregate.test_parser("(1, 2, 3, 4)")),
            "Aggregate([(None, Num(1)), (None, Num(2)), (None, Num(3)), "
            "(None, Num(4))] | ancestor_expr=None)")

        self.assertEqual(
            str(A.aggregate.test_parser("(A => 1, B => 2, C => 3, others => "
                                       "4)")),
            "Aggregate([([Id(A)], Num(1)), ([Id(B)], Num(2)), "
            "([Id(C)], Num(3)), (Kw('others'), Num(4))] | ancestor_expr=None)")

        self.assertEqual(
            str(A.aggregate.test_parser("(A => 1, B | E => 2, C => 3)")),
            "Aggregate([([Id(A)], Num(1)), ([Id(B), Id(E)], Num(2)), "
            "([Id(C)], Num(3))] | ancestor_expr=None)")

        self.assertEqual(
            str(A.aggregate.test_parser("(1 => 1, 2 .. 5 => 2, others => 4)")),
            "Aggregate([([Num(1)], Num(1)), ([BinOp(Num(2), Punc('..'), "
            "Num(5))], Num(2)), (Kw('others'), Num(4))] | ancestor_expr=None)")

        self.assertEqual(
            str(A.aggregate.test_parser("(Test_Record with A => 1, B => 2)")),
            "Aggregate([([Id(A)], Num(1)), ([Id(B)], Num(2))] "
            "| ancestor_expr=Id(Test_Record))"
        )

    def test_name(self):

        self.assertEqual(
            str(A.name.test_parser("A.B.C.D")),
            "DotEpxression([Id(A), Id(B), Id(C), Id(D)])")

        self.assertEqual(
            str(A.name.test_parser("A.B(12, 15)")),
            "DotEpxression([Id(A), CallExpr(Id(B), [(None, Num(12)), "
            "(None, Num(15))])])"
        )

        print A.discrete_range.test_parser("12 .. 15")
        print A.name.test_parser("A(12 .. 15)")


class SubpTest(unittest.TestCase):

    def test(self):
        subp_spec = A.subprogram_spec.test_parser(
            "procedure A (B : in out Integer; C, D : access String)"
        )

        print subp_spec.name
        self.assertEqual(
            "SubprogramSpec(QualName(A), [ParameterProfile([Id(B)], "
            "3, TypeRef(Id(Integer)) | aliased=False), "
            "ParameterProfile([Id(C), Id(D)], 1, TypeAccessExpression(Id("
            "String) | is_constant=False) | aliased=False)], None)",
            str(subp_spec)
        )

        t2 = A.subprogram_decl.test_parser(
            "overriding procedure A (B : in out Integer; "
            "C, D : access String);"
        )

    def test_bench(self):

        def test2():
            from time import time
            procedure_str = ";\n".join(
                "procedure a (b : in out integer; c, d, e, f, g, h, i : access"
                " string)" for _ in range(150000)
            )
            t = time()
            tkz = make_ada_tokenizer(procedure_str)
            print "LEXER : ", time() - t

            t2 = time()
            import test_parser
            test_parser.t_list_parse_3(tkz, 0)
            print "PARSER : ", time() - t2
            print "TOTAL : ", time() - t
            print

        def test():
            from time import time
            import test_parser
            t = time()
            tkz = make_ada_tokenizer(
                "procedure a (b : in out integer; c, d, e, f, g, h, i : access "
                "string)"
            )

            for _ in range(1000000):
                test_parser.t_transform_parse_2(tkz, 0)

            print time() - t

        # for i in range(10):
        #     test2()

        # import cProfile
        # cProfile.runctx("test2()", globals=globals(), locals=locals(),
        # sort=1)

