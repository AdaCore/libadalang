from langkit.compiled_types import Field, abstract, EnumType
from langkit.parsers import Opt, List, Or, Row, Enum, Tok, Null

from language.parser import A, AdaNode
from language.parser.lexer import Token


@abstract
class Expr(AdaNode):
    pass


class UnOp(Expr):
    op = Field()
    expr = Field()


class BinOp(Expr):
    left = Field()
    op = Field()
    right = Field()


class MembershipExpr(Expr):
    expr = Field()
    op = Field()
    membership_exprs = Field()


class Aggregate(Expr):
    ancestor_expr = Field()
    assocs = Field()


class NameExpr(Expr):
    expr_list = Field()


class StaticNameExpr(Expr):
    id_list = Field()


class CallExpr(Expr):
    name = Field()
    paren_tok = Field(repr=False)
    suffix = Field()


class ExprList(AdaNode):
    exprs = Field()


class ParamAssoc(AdaNode):
    designator = Field()
    expr = Field()


class ParamList(AdaNode):
    params = Field()


class AccessDeref(Expr):
    pass


class DiamondExpr(Expr):
    pass


class AggregateField(AdaNode):
    pass


class OthersDesignator(AdaNode):
    pass


class AggregateMember(AdaNode):
    choice_list = Field()


class Op(EnumType):
    alternatives = ["and", "or", "or_else", "and_then", "xor", "in",
                    "not_in", "abs", "not", "pow", "mult", "div", "mod",
                    "rem", "plus", "minus", "bin_and", "eq", "neq", "lt",
                    "lte", "gt", "gte", "ellipsis"]
    suffix = 'op'


class IfExpr(Expr):
    cond_expr = Field()
    then_expr = Field()
    elsif_list = Field()
    else_expr = Field()


class ElsifExprPart(AdaNode):
    cond_expr = Field()
    then_expr = Field()


class CaseExpr(Expr):
    expr = Field()
    cases = Field()


class CaseExprAlternative(Expr):
    choices = Field()
    expr = Field()


@abstract
class SingleTokNode(Expr):
    tok = Field()


class Identifier(SingleTokNode):
    _repr_name = "Id"


class EnumIdentifier(Identifier):
    _repr_name = "EnumId"


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
    ids = Field()


class Quantifier(EnumType):
    alternatives = ["all", "some"]
    suffix = 'items'


class IterType(EnumType):
    alternatives = ["in", "of"]
    suffix = 'iter'


@abstract
class LoopSpec(AdaNode):
    pass


class ForLoopSpec(LoopSpec):
    id = Field()
    loop_type = Field()
    is_reverse = Field()
    iter_expr = Field()


class QuantifiedExpr(Expr):
    quantifier = Field()
    loop_spec = Field()
    expr = Field()


class QualifiedExpr(Expr):
    subtype_mark = Field()
    expr = Field()


class Allocator(Expr):
    subpool = Field()
    expr = Field()


class QualExpr(Expr):
    prefix = Field()
    suffix = Field()


@abstract
class AbstractAggregateContent(AdaNode):
    pass


class AggregateContent(AbstractAggregateContent):
    fields = Field()


class AggregateAssoc(AdaNode):
    designator = Field()
    expr = Field()


class Prefix(Expr):
    prefix = Field()
    suffix = Field()


class AttributeRef(Expr):
    prefix = Field()
    attribute = Field()
    args = Field()


class RaiseExpression(Expr):
    exception_name = Field()
    error_message = Field()


A.add_rules(
    identifier=Tok(Token.Identifier, keep=True) ^ Identifier,
    enum_identifier=Tok(Token.Identifier, keep=True) ^ EnumIdentifier,
    char_literal=Tok(Token.Char, keep=True) ^ CharLiteral,
    string_literal=Tok(Token.String, keep=True) ^ StringLiteral,
    num_literal=Tok(Token.Number, keep=True) ^ NumLiteral,
    null_literal=Tok(Token.Null, keep=True) ^ NullLiteral,

    allocator=Row(
        "new", Opt("(", A.name, ")")[1], A.type_expression | A.name
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
        A.for_loop_parameter_spec, "=>",
        A.expression | A.discrete_range
    ) ^ QuantifiedExpr,

    attribute=Or(
        Or("access", "delta", "digits", "mod", "range") ^ Attribute,
        A.identifier
    ),

    case_expression=Row(
        "case", A.expression, "is",
        List(A.case_expr_alt, sep=",")
    ) ^ CaseExpr,

    case_expr_alt=Row(
        "when", A.choice_list, "=>", A.expression
    ) ^ CaseExprAlternative,


    raise_expression=Or(
        Row("raise", A.name, Opt("with", A.expression)[1]) ^ RaiseExpression,
        Row("raise", Null(Expr), Null(Expr)) ^ RaiseExpression,
    ),

    if_expression=Row(
        "if", A.expression, "then", A.expression,
        List(Row("elsif", A.expression,
                 "then", A.expression) ^ ElsifExprPart, empty_valid=True),
        Opt("else", A.expression)[1],
    ) ^ IfExpr,

    conditional_expression=Or(A.if_expression, A.case_expression,
                              A.quantified_expression),

    diamond_expr=Tok("<>") ^ DiamondExpr,

    others_designator=Tok("others") ^ OthersDesignator,

    aggregate_field=Or(
        A.choice_list ^ AggregateMember,
        A.expression,
        A.others_designator,
    ),

    aggregate_assoc=Row(
        Opt(A.aggregate_field, "=>")[0],
        Or(A.diamond_expr, A.expression)
    ) ^ AggregateAssoc,
    aggregate_content=List(A.aggregate_assoc, sep=",") ^ AggregateContent,
    aggregate_content_null=Row(
        "null", "record", Null(AggregateContent)
    )[2],

    positional_aggregate=List(A.expression, sep=","),

    aggregate=Row(
        "(",
        Row(
            Opt(A.expression, "with")[0],
            Or(A.aggregate_content_null, A.aggregate_content)
        ) ^ Aggregate,
        ")")[1],

    direct_name=Or(A.identifier, A.string_literal, A.char_literal,
                   A.access_deref, A.attribute),

    param_assoc=Row(
        Opt(A.identifier | A.others_designator | A.string_literal,
            "=>")[0],
        A.expression | A.diamond_expr
    ) ^ ParamAssoc,

    call_suffix=Or(
        A.discrete_range,
        List(A.param_assoc, sep=",")
        ^ ParamList
    ),

    name=Or(
        Row(A.name, Tok("(", keep=True), A.call_suffix, ")") ^ CallExpr,
        Row(A.name, ".", A.direct_name) ^ Prefix,
        Row(A.name, "'", A.attribute,
            Opt("(", A.call_suffix, ")")[1]) ^ AttributeRef,
        Row(A.name, "'",
            Or(Row("(", A.expression, ")")[1], A.aggregate)) ^ QualExpr,
        A.direct_name,
    ),

    access_deref=Tok("all") ^ AccessDeref,

    type_name=List(A.direct_name, sep=".", revtree=Prefix),

    static_name=List(A.identifier, sep=".", revtree=Prefix),

    primary=Or(A.num_literal, A.null_literal,
               A.name, A.allocator,
               A.conditional_expression,
               A.raise_expression,
               Row("(", A.conditional_expression | A.expression, ")")[1],
               A.aggregate),

    factor=Or(
        Row(Or(Enum("abs", Op("abs")), Enum("not", Op("not"))),
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
        Enum("xor", Op("xor")),
        Enum(Row("and", "then"), Op("and_then")), Enum("and", Op("and")),
        Enum(Row("or", "else"), Op("or_else")), Enum("or", Op("or")),
    ),

    expression_list=Row("(", List(A.expression, sep=","), ")") ^ ExprList,

    discrete_range=Row(A.expression,
                       Enum("..", Op("ellipsis")), A.expression) ^ BinOp,

    range_expression=Or(
        A.discrete_range
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
