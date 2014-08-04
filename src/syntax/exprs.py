from copy import deepcopy
from combinators import Opt, List, Or, Row, _, EnumType, Enum, Tok, \
    TokClass, Null
from syntax import ASTNode, A, Field, TokenType
from tokenizer import Token, Id, CharLit, StringLit, NumLit, Lbl
from utils import extract


class Expr(ASTNode):
    abstract = True


class UnOp(Expr):
    fields = [
        Field("op", tk_start=True, repr=True),
        Field("expr", tk_end=True, repr=True)
    ]


class BinOp(Expr):
    fields = [
        Field("left", tk_start=True, repr=True),
        Field("op", repr=True),
        Field("right", tk_end=True, repr=True)
    ]


class AttributeExpr(Expr):
    fields = [
        Field("expr", repr=True),
        Field("attr", repr=True),
        Field("args", repr=True),
        Field("tk_end")
    ]


class MembershipExpr(Expr):
    fields = [
        Field("expr", repr=True),
        Field("op", repr=True),
        Field("membership_exprs", repr=True)
    ]


class Aggregate(Expr):
    fields = [
        Field("ancestor_expr", kw_repr=True),
        Field("assocs", repr=True),
        Field("close_par", tk_end=True)
    ]


class NameExpr(Expr):
    fields = [
        Field("expr_list", repr=True)
    ]


class StaticNameExpr(Expr):
    fields = [
        Field("id_list", repr=True)
    ]


class CallExpr(Expr):
    fields = [
        Field("prefix", tk_start=True, repr=True),
        Field("calls", repr=True),
    ]


class ExprList(ASTNode):
    fields = [
        Field("tk_start"),
        Field("exprs", repr=True),
        Field("tk_end")
    ]


class ParamList(ASTNode):
    fields = [
        Field("params", repr=True)
    ]


class AccessDeref(Expr):
    fields = [Field("token")]


class DiamondExpr(Expr):
    fields = [Field("token")]


class AggregateField(ASTNode):
    pass


class OthersDesignator(ASTNode):
    fields = [Field("token")]


class AggregateMember(ASTNode):
    fields = [Field("choice_list", repr=True)]


class Op(EnumType):
    alternatives = ["_and", "_or", "or_else", "and_then", "_xor", "in",
                    "not_in", "abs", "_not", "pow", "mult", "div", "mod",
                    "rem", "plus", "minus", "bin_and", "eq", "neq", "lt",
                    "lte", "gt", "gte", "ellipsis"]


class IfExpr(Expr):
    fields = [
        Field("if_kw"),
        Field("cond_expr", repr=True),
        Field("then_expr", repr=True),
        Field("elsif_list", repr=True),
        Field("else_expr", repr=True),
        Field("tok_end")
    ]


class CaseExpr(Expr):
    fields = [
        Field("expr", repr=True),
        Field("cases", repr=True)
    ]


class SingleTokNode(Expr):
    fields = [
        Field("tok", repr=True, tk_start=True, tk_end=True)
    ]


class Identifier(SingleTokNode):
    _repr_name = "Id"


class CharLiteral(SingleTokNode):
    _repr_name = "Chr"


class StringLiteral(SingleTokNode):
    _repr_name = "Str"


class NumLiteral(SingleTokNode):
    _repr_name = "Num"


class NullLiteral(SingleTokNode):
    _repr_name = "Null"


class Attribute(SingleTokNode):
    _repr_name = "Attr"


class QualifiedName(Expr):
    fields = [Field("ids", repr=True)]


class Quantifier(EnumType):
    alternatives = ["all", "some"]


class IterType(EnumType):
    alternatives = ["in", "of"]


class LoopSpec(ASTNode):
    abstract = True


class ForLoopSpec(LoopSpec):
    fields = [
        Field("id", repr=True),
        Field("loop_type", repr=True),
        Field("is_reverse", repr=True),
        Field("iter_expr", repr=True)
    ]


class QuantifiedExpr(Expr):
    fields = [
        Field("for_kw"),
        Field("quantifier", repr=True),
        Field("loop_spec", repr=True),
        Field("expr", repr=True)
    ]


class QualifiedExpr(Expr):
    fields = [Field("subtype_mark", repr=True), Field("expr", repr=True)]


class Allocator(Expr):
    fields = [
        Field("new_kw"),
        Field("subpool", repr=True),
        Field("expr", repr=True)
    ]


class NameComponent(Expr):
    fields = [
        Field("list", repr=True),
    ]


A.add_rules(
    identifier=TokClass(Id) ^ Identifier,
    qualified_name=List(A.identifier, sep=".") ^ QualifiedName,
    char_literal=TokClass(CharLit) ^ CharLiteral,
    string_literal=TokClass(StringLit) ^ StringLiteral,
    num_literal=TokClass(NumLit) ^ NumLiteral,
    null_literal=Tok(Token("null")) ^ NullLiteral,

    allocator=Row(
        "new", Opt("(", A.name, ")") >> 1, A.type_expression | A.name
    ) ^ Allocator,

    for_loop_parameter_spec=Row(
        A.identifier,
        Or(Enum("in", IterType("in")), Enum("of", IterType("of"))),
        Opt("reverse").as_bool(),
        A.constrained_type_ref | A.discrete_range | A.expression
    ) ^ ForLoopSpec,

    quantified_expression=Row(
        "for", Or(Enum("all", Quantifier("all")),
                  Enum("some", Quantifier("some"))),
        A.for_loop_parameter_spec, _("=>"),
        A.expression | A.discrete_range
    ) ^ QuantifiedExpr,

    attribute=Or(
        Or("access", "delta", "digits", "mod", "range") ^ Attribute,
        A.identifier
    ),

    case_expression=Row(
        _("case"), A.expression, _("is"),
        List(A.case_expr_alt, sep=",")
    ) ^ CaseExpr,

    case_expr_alt=Row(
        _("when"), A.choice_list, _("=>"), A.expression
    ),

    if_expression=Row(
        "if", A.expression, _("then"), A.expression,
        List(Row(_("elsif"), A.expression,
                 _("then"), A.expression), empty_valid=True),
        Opt("else", A.expression) >> 1,
    ) ^ IfExpr,

    conditional_expression=Or(A.if_expression, A.case_expression,
                              A.quantified_expression),

    diamond_expr=Tok(Token("<>")) ^ DiamondExpr,

    others_designator=Tok(Token("others")) ^ OthersDesignator,

    aggregate_field=Or(
        A.expression,
        A.choice_list ^ AggregateMember,
        A.others_designator,
    ),

    aggregate_assoc = Row(
            Opt(A.aggregate_field, _("=>")) >> 0,
            Or(A.diamond_expr, A.expression)
    ),
    aggregate_content_empty_valid=List(A.aggregate_assoc, sep=",",
                                       empty_valid=True),
    aggregate_content=List(A.aggregate_assoc, sep=","),

    positional_aggregate=List(A.expression, sep=","),

    aggregate=Row(
        "(",
        Or(Row(A.expression, _("with"),
               _(Opt("null", "record")),
               A.aggregate_content_empty_valid) ^ Aggregate,
           Row(Null(Expr), A.aggregate_content) ^ Aggregate),
        ")"
    ) >> 1,

    direct_name=Or(A.identifier, A.string_literal, A.char_literal,
                   A.access_deref, A.attribute),

    call_suffix=Or(
        A.discrete_range,
        List(Row(Opt(A.identifier | A.others_designator | A.string_literal,
                     "=>") >> 0,
                 A.expression | A.diamond_expr), sep=",")
        ^ ParamList
    ),

    _name_component=Or(
        Row(A.direct_name,
            List(Row("(", A.call_suffix, ")") >> 1)) ^ CallExpr,
        A.direct_name,
    ),

    qual_expr_content=Or(Row("(", A.expression, ")") >> 1, A.aggregate),

    name_component=List(A._name_component | A.qual_expr_content, sep="'") ^ NameComponent,

    name=Row(List(A.name_component | A.direct_name, sep=".")) ^ NameExpr,

    access_deref=Tok(Token("all")) ^ AccessDeref,

    type_name=Row(List(A.direct_name, sep=".")) ^ NameExpr,

    static_name=Row(List(A.identifier, sep=".")) ^ StaticNameExpr,

    primary=Or(A.num_literal, A.null_literal, A.string_literal,
               A.name, A.allocator,
               A.conditional_expression,
               Row("(", A.conditional_expression | A.expression, ")") >> 1,
               A.aggregate),

    factor=Or(
        Row(Or(Enum("abs", Op("abs")), Enum("not", Op("_not"))),
            A.primary) ^ UnOp,

        Row(A.primary, Enum("**", Op("pow")), A.primary) ^ BinOp,

        A.primary
    ),

    term=Or(
        Row(A.factor, Or(Enum("*", Op("mult")),
                         Enum("/", Op("div")),
                         Enum("mod", Op("mod")),
                         Enum("rem", Op("rem"))), A.term) ^ BinOp,
        A.factor
    ),

    unop_term=Or(
        Row(Or(Enum("+", Op("plus")),
               Enum("-", Op("minus"))),
            A.term) ^ UnOp,
        A.term
    ),

    simple_expr=Or(
        Row(A.unop_term, Or(Enum("+", Op("plus")),
                            Enum("-", Op("minus")),
                            Enum("&", Op("bin_and"))),
            A.simple_expr_2) ^ BinOp,
        A.unop_term
    ),

    simple_expr_2=Or(
        Row(A.term, Or(Enum("+", Op("plus")),
                       Enum("-", Op("minus")),
                       Enum("&", Op("bin_and"))),
            A.simple_expr_2) ^ BinOp,
        A.term
    ),

    boolean_op=Or(
        Enum("xor", Op("_xor")),
        Enum(Row("and", "then"), Op("and_then")), Enum("and", Op("_and")),
        Enum(Row("or", "else"), Op("or_else")), Enum("or", Op("_or")),
    ),

    expression_list=Row("(", List(A.expression, sep=","), ")") ^ ExprList,

    discrete_range=Row(A.expression,
                       Enum("..", Op("ellipsis")), A.expression) ^ BinOp,

    range_expression=Or(
        A.discrete_range, A.name
    ),

    choice=Or(A.range_expression, A.expression, A.others_designator),

    choice_list=List(A.choice, sep="|"),

    rel_op=Or(
        Enum(Row("not", "in"), Op("not_in")),
        Enum("in", Op("in")),
    ),

    relation=Or(
        Row(A.simple_expr,
            Or(Enum("=", Op("eq")), Enum("/=", Op("neq")),
               Enum("<", Op("lt")), Enum("<=", Op("lte")),
               Enum(">", Op("gt")), Enum(">=", Op("gte"))),
            A.relation) ^ BinOp,

        Row(A.simple_expr, A.rel_op, A.choice_list)
        ^ MembershipExpr,

        A.simple_expr
    ),

    expression=Or(
        Row(A.relation, A.boolean_op, A.expression) ^ BinOp,
        A.relation
    ),
)
