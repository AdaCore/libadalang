from copy import deepcopy
from parsers import abstract, Opt, List, Or, Row, _, EnumType, Enum, Tok, \
    TokClass, Null, Success, ASTNode, Field, TokenType
from ada_parser import A
from tokenizer import Token, Id, CharLit, StringLit, NumLit, Lbl
from utils import extract


@abstract
class Expr(ASTNode):
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
    prefix = Field()
    calls = Field()


class ExprList(ASTNode):
    tk_start = Field()
    exprs = Field()
    tk_end = Field()


class ParamList(ASTNode):
    params = Field()


class AccessDeref(Expr):
    token = Field(repr=False)


class DiamondExpr(Expr):
    token = Field(repr=False)


class AggregateField(ASTNode):
    pass


class OthersDesignator(ASTNode):
    token = Field(repr=False)


class AggregateMember(ASTNode):
    choice_list = Field()


class Op(EnumType):
    alternatives = ["_and", "_or", "or_else", "and_then", "_xor", "in",
                    "not_in", "abs", "_not", "pow", "mult", "div", "mod",
                    "rem", "plus", "minus", "bin_and", "eq", "neq", "lt",
                    "lte", "gt", "gte", "ellipsis"]


class IfExpr(Expr):
    if_kw = Field(repr=False)
    cond_expr = Field()
    then_expr = Field()
    elsif_list = Field()
    else_expr = Field()
    tok_end = Field(repr=False)


class CaseExpr(Expr):
    expr = Field()
    cases = Field()


@abstract
class SingleTokNode(Expr):
    tok = Field()


class Identifier(SingleTokNode):
    _repr_name = "Id"

    # properties = {
    #     "type": ParentProperty("type")
    # }



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


class IterType(EnumType):
    alternatives = ["in", "of"]


@abstract
class LoopSpec(ASTNode):
    pass


class ForLoopSpec(LoopSpec):
    id = Field()
    loop_type = Field()
    is_reverse = Field()
    iter_expr = Field()


class QuantifiedExpr(Expr):
    for_kw = Field(repr=False)
    quantifier = Field()
    loop_spec = Field()
    expr = Field()


class QualifiedExpr(Expr):
    subtype_mark = Field()
    expr = Field()


class Allocator(Expr):
    new_kw = Field(repr=False)
    subpool = Field()
    expr = Field()


class QualExpr(Expr):
    prefix = Field()
    suffix = Field()


@abstract
class AbstractAggregateContent(ASTNode):
    pass


class NullAggregateContent(AbstractAggregateContent):
    pass


class AggregateContent(AbstractAggregateContent):
    fields = Field()


class Prefix(Expr):
    prefix = Field()
    suffix = Field()


class AttributeRef(Expr):
    prefix = Field()
    attribute = Field()
    args = Field()


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
        A.choice_list ^ AggregateMember,
        A.expression,
        A.others_designator,
    ),

    aggregate_assoc=Row(
        Opt(A.aggregate_field, _("=>")) >> 0,
        Or(A.diamond_expr, A.expression)
    ),
    aggregate_content=List(A.aggregate_assoc, sep=",") ^ AggregateContent,
    aggregate_content_null=Row(
        "null", "record", Success(NullAggregateContent)
    ) >> 2,

    positional_aggregate=List(A.expression, sep=","),

    aggregate=Row(
        "(",
        Row(
            Opt(A.expression, "with") >> 0,
            Or(A.aggregate_content_null, A.aggregate_content)
        ) ^ Aggregate
        , ")") >> 1,

    direct_name=Or(A.identifier, A.string_literal, A.char_literal,
                   A.access_deref, A.attribute),

    call_suffix=Or(
        A.discrete_range,
        List(Row(Opt(A.identifier | A.others_designator | A.string_literal,
                     "=>") >> 0,
                 A.expression | A.diamond_expr), sep=",")
        ^ ParamList
    ),

    name=Or(
        Row(A.name, _("("), A.call_suffix, _(")")) ^ CallExpr,
        Row(A.name, _("."), A.direct_name) ^ Prefix,
        Row(A.name, _("'"), A.attribute, Opt("(", A.call_suffix, ")") >> 1) ^ AttributeRef,
        Row(A.name, _("'"),
            Or(Row("(", A.expression, ")") >> 1, A.aggregate)) ^ QualExpr,
        A.direct_name,
    ),

    access_deref=Tok(Token("all")) ^ AccessDeref,

    type_name=List(A.direct_name, sep=".", revtree=Prefix),

    static_name=List(A.identifier, sep=".", revtree=Prefix),

    primary=Or(A.num_literal, A.null_literal,
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
