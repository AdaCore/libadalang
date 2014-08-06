from syntax import ASTNode, A, Field
from combinators import Opt, List, Or, Row, _, EnumType, Enum


class WithDecl(ASTNode):
    fields = [
        Field("is_limited", repr=True),
        Field("is_private", repr=True),
        Field("packages", repr=True)
    ]


class UseDecl(ASTNode):
    pass


class UsePkgDecl(UseDecl):
    fields = [
        Field("start_token"),
        Field("packages", repr=True)
    ]


class UseTypDecl(UseDecl):
    fields = [
        Field("start_token"),
        Field("all", repr=True),
        Field("types", repr=True)
    ]


class TypeExpression(ASTNode):
    """
    This type will be used as a base for what represents a type expression
    in the Ada syntax tree.
    """
    fields = [
        Field("null_exclusion", repr=True),
        Field("type_expr_variant", repr=True, norepr_null=True)
    ]


class TypeExprVariant(ASTNode):
    abstract = True


class TypeRef(TypeExprVariant):
    fields = [
        Field("name", repr=True),
        Field("constraint", repr=True, opt=True)
    ]


class AccessExpression(TypeExprVariant):
    abstract = True


class SubprogramAccessExpression(AccessExpression):
    fields = [
        Field("access_token"),
        Field("is_protected", kw_repr=True),
        Field("subp_spec", repr=True)
    ]


class TypeAccessExpression(AccessExpression):
    fields = [
        Field("is_all", repr=True),
        Field("is_constant", repr=True),
        Field("subtype_name", repr=True)
    ]


class ParameterProfile(ASTNode):
    fields = [
        Field("ids", repr=True),
        Field("is_aliased", kw_repr=True),
        Field("mode", repr=True),
        Field("type_expr", repr=True),
        Field("_default", repr=True)
    ]


class AspectSpecification(ASTNode):
    fields = [
        Field("aspect_assocs", repr=True)
    ]


class SubprogramSpec(ASTNode):
    fields = [
        Field("tk_start"),
        Field("name", repr=True),
        Field("params", repr=True),
        Field("returns", repr=True),
    ]


class SubprogramDecl(ASTNode):
    fields = [
        Field("is_overriding", repr=True),
        Field("subp_spec", repr=True),
        Field("is_null", repr=True),
        Field("is_abstract", repr=True),
        Field("expression", repr=True),
        Field("renames", repr=True),
        Field("aspects"),
    ]


class Pragma(ASTNode):
    fields = [
        Field("pragma_kw"),
        Field("id", repr=True),
        Field("args", repr=True),
    ]


# #####################
# GRAMMAR DEFINITION #
######################

class InOut(EnumType):
    alternatives = ["in", "out", "inout"]


class AspectClause(ASTNode):
    pass


class EnumRepClause(AspectClause):
    fields = [
        Field("type_name", repr=True),
        Field("aggregate", repr=True)
    ]


class AttributeDefClause(AspectClause):
    fields = [
        Field("attribute_expr", repr=True),
        Field("expr", repr=True)
    ]


class RecordRepComponent(ASTNode):
    fields = [
        Field("id", repr=True),
        Field("position", repr=True),
        Field("range", repr=True)
    ]


class RecordRepClause(AspectClause):
    fields = [
        Field("component_name", repr=True),
        Field("at_expr", repr=True),
        Field("components", repr=True)
    ]


class AtClause(AspectClause):
    fields = [
        Field("name", repr=True),
        Field("expr", repr=True),
    ]


class EntryDecl(ASTNode):
    fields = [
        Field("overriding", repr=True),
        Field("entry_id", repr=True),
        Field("family_type", repr=True),
        Field("params", repr=True),
        Field("aspects", repr=True)
    ]


class TaskDecl(ASTNode):
    fields = [
        Field("task_name", repr=True),
        Field("aspects", repr=True),
        Field("def", repr=True)
    ]


class ProtectedDecl(ASTNode):
    fields = [
        Field("protected_name", repr=True),
        Field("aspects", repr=True),
        Field("def", repr=True)
    ]


class AspectAssoc(ASTNode):
    fields = [
        Field("id", repr=True),
        Field("expr", repr=True)
    ]


class NumberDecl(ASTNode):
    fields = [
        Field("ids", repr=True),
        Field("expr", repr=True)
    ]


class ObjectDecl(ASTNode):
    fields = [
        Field("ids", repr=True),
        Field("aliased", repr=True),
        Field("constant", repr=True),
        Field("inout", repr=True),
        Field("type", repr=True),
        Field("default_expr", repr=True),
        Field("renaming_clause", repr=True),
        Field("aspects", repr=True)
    ]


class PackageDecl(ASTNode):
    fields = [
        Field("name", repr=True),
        Field("aspects", repr=True),
        Field("decls", repr=True),
        Field("private_decls", repr=True),
    ]


class ExceptionDecl(ASTNode):
    fields = [
        Field("ids", repr=True),
        Field("renames", repr=True),
        Field("aspects", repr=True)
    ]


class GenericInstantiation(ASTNode):
    fields = [
        Field("type_token"),
        Field("name", repr=True),
        Field("generic_entity_name", repr=True),
        Field("parameters", repr=True),
        Field("aspects", repr=True)
    ]


class RenamingClause(ASTNode):
    fields = [Field("renamed_object", repr=True)]


class PackageRenamingDecl(ASTNode):
    fields = [
        Field("name", repr=True),
        Field("renames", repr=True),
        Field("aspects", repr=True),
    ]


class GenericRenamingDecl(ASTNode):
    fields = [
        Field("name", repr=True),
        Field("renames", repr=True),
        Field("aspects", repr=True)
    ]


class FormalSubpDecl(ASTNode):
    fields = [
        Field("subp_spec", repr=True),
        Field("is_abstract", repr=True),
        Field("default_value", repr=True)
    ]


class Overriding(EnumType):
    alternatives = ["overriding", "not_overriding", "unspecified"]


class GenericSubprogramDecl(ASTNode):
    fields = [
        Field("formal_part", repr=True),
        Field("subp_spec", repr=True)
    ]


class GenericPackageDecl(ASTNode):
    fields = [
        Field("formal_part", repr=True),
        Field("package_decl", repr=True)
    ]


A.add_rules(
    generic_decl=Or(
        Row(A.generic_formal_part, A.subprogram_spec) ^ GenericSubprogramDecl,
        Row(A.generic_formal_part, A.package_decl) ^ GenericPackageDecl
    ),

    generic_formal_part=Row(
        "generic", List(Row(A.generic_formal_decl | A.use_decl, ";") >> 0,
                        empty_valid=True)
    ) >> 1,

    generic_formal_decl=Or(
        A.pragma,
        A.object_decl,
        A.full_type_decl,
        A.formal_subp_decl,
        Row("with", A.generic_instantiation) >> 1
    ),

    formal_subp_decl=Row(
        _("with"),
        A.subprogram_spec,
        _(Opt("is")),
        Opt("abstract").as_bool(),
        Opt(Or(A.diamond_expr, A.static_name, A.null_literal))
    ) ^ FormalSubpDecl,

    renaming_clause=Row(_("renames"), A.name) ^ RenamingClause,

    generic_renaming_decl=Row(
        _("generic"), _(Or("package", "function", "procedure")), A.static_name,
        _("renames"), A.static_name, A.aspect_specification
    ) ^ GenericRenamingDecl,

    generic_instantiation=Row(
        Or("package", "function", "procedure"), A.static_name, _("is"),
        _("new"), A.static_name,
        Opt("(", A.call_suffix, ")") >> 1,
        A.aspect_specification
    ) ^ GenericInstantiation,

    exception_decl=Row(
        A.id_list, _(":"), _("exception"),
        Opt(A.renaming_clause), A.aspect_specification
    ) ^ ExceptionDecl,

    basic_decls=List(Row(A.basic_decl, ";") >> 0, empty_valid=True),

    package_renaming_decl=Row(
        _("package"), A.static_name, A.renaming_clause, A.aspect_specification
    ) ^ PackageRenamingDecl,

    package_decl=Row(
        _("package"), A.static_name, A.aspect_specification, _("is"),
        A.basic_decls,
        Opt("private", A.basic_decls) >> 1,
        _("end"), _(A.static_name)
    ) ^ PackageDecl,

    basic_decl=Or(
        A.body,
        A.body_stub,
        A.full_type_decl,
        A.task_type_decl,
        A.protected_type_decl,
        A.subprogram_decl,
        A.subtype_decl,
        A.object_decl,
        A.package_decl,
        A.generic_instantiation,
        A.aspect_clause,
        A.use_decl,
        A.exception_decl,
        A.package_renaming_decl,
        A.generic_renaming_decl,
        A.generic_decl,
        A.pragma
    ),

    object_decl=Or(
        A._object_decl,
        A.task_decl,
        A.protected_decl,
        A.number_decl
    ),

    _object_decl=Row(
        A.id_list,  _(":"),
        Opt("aliased").as_bool(),
        Opt("constant").as_bool(),
        Opt(A.in_out),
        A.type_expression | A.array_type_def,
        A.default_expr,
        Opt(A.renaming_clause),
        A.aspect_specification
    ) ^ ObjectDecl,

    id_list=List(A.identifier, sep=","),

    number_decl=Row(
        A.id_list, _(":"), _("constant"), _(":="),
        A.simple_expr
    ) ^ NumberDecl,

    aspect_assoc=Row(
        A.name, Opt("=>", A.expression) >> 1
    ) ^ AspectAssoc,

    aspect_specification=Opt(
        Row(
            _("with"),
            List(A.aspect_assoc, sep=",")
        ) ^ AspectSpecification
    ),

    protected_decl=Row(
        _("protected"), A.identifier, A.aspect_specification,
        _("is"), A.protected_def
    ) ^ ProtectedDecl,

    task_decl=Row(_("task"), A.identifier, A.aspect_specification,
                  _("is"), A.task_def) ^ TaskDecl,

    overriding_indicator=Or(
        Enum("overriding", Overriding("overriding")),
        Enum(Row("not", "overriding"), Overriding("not_overriding")),
        Enum(None, Overriding("unspecified"))
    ),

    entry_decl=Row(
        A.overriding_indicator,
        _("entry"),
        A.identifier,
        Opt("(", A.type_ref, ")") >> 1,
        Opt(A.parameter_profiles),
        A.aspect_specification
    ) ^ EntryDecl,


    rep_component_clause=Row(
        A.identifier, _("at"), A.simple_expr, A.range_spec
    ) ^ RecordRepComponent,

    aspect_clause=Or(
        Row(_("for"), A.name, _("use"), A.expression)
        ^ AttributeDefClause,

        Row(_("for"), A.static_name, _("use"), A.aggregate)
        ^ EnumRepClause,

        Row(_("for"), A.static_name, _("use"), _("record"),
            Opt("at", "mod", A.simple_expr) >> 2,
            List(Row(A.rep_component_clause, ";") >> 0, empty_valid=True),
            _("end"), _("record")) ^ RecordRepClause,

        Row(_("for"), A.direct_name, _("use"), _("at"), A.expression)
        ^ AtClause
    ),

    parameter_profile=Row(
        List(A.identifier, sep=","),
        _(":"),
        Opt("aliased").as_bool(),
        Opt(A.in_out),
        A.type_expression,
        Opt(":=", A.expression) >> 1,
    ) ^ ParameterProfile,

    parameter_profiles=Row("(", List(A.parameter_profile, sep=";"), ")") >> 1,

    subprogram_spec=Row(
        Or("procedure", "function"),
        Opt(A.name),
        Opt(A.parameter_profiles),
        Opt("return", A.type_expression) >> 1
    ) ^ SubprogramSpec,

    test_spec=List(A.subprogram_spec, sep=";"),

    subprogram_decl=Row(
        A.overriding_indicator,
        A.subprogram_spec,
        Opt("is", "null").as_bool(),
        Opt("is", "abstract").as_bool(),
        Opt("is", A.expression) >> 1,
        Opt(A.renaming_clause),
        A.aspect_specification,
    ) ^ SubprogramDecl,

    type_access_expression=Row(
        _("access"),
        Opt("all").as_bool(),
        Opt("constant").as_bool(), A.name
    ) ^ TypeAccessExpression,

    with_decl=Row(
        Opt("limited").as_bool(),
        Opt("private").as_bool(),
        _("with"), List(A.static_name, sep=",")
    ) ^ WithDecl,

    context_item=Or(A.with_decl, A.use_decl, A.pragma),

    use_decl=Or(A.use_package_decl, A.use_type_decl),

    use_package_decl=Row("use", List(A.static_name, sep=",")) ^ UsePkgDecl,

    use_type_decl=Row("use", Opt("all").as_bool(), _("type"),
                      List(A.name, sep=",")) ^ UseTypDecl,

    subprogram_access_expression=Row(
        "access", Opt("protected").as_bool(), A.subprogram_spec
    ) ^ SubprogramAccessExpression,

    access_expression=Or(A.subprogram_access_expression,
                         A.type_access_expression),

    type_ref=Row(
        A.name, Opt(A.constraint)
    ) ^ TypeRef,

    constrained_type_ref=Row(
        A.name, A.constraint
    ) ^ TypeRef,

    type_expression=Row(
        Opt("not", "null").as_bool(),
        Or(A.access_expression, A.type_ref),
    ) ^ TypeExpression,

    in_out=Or(
        Enum(Row("in", "out"), InOut("inout")),
        Enum("in", InOut("in")),
        Enum("out", InOut("out")),
        Enum(None, InOut("in"))
    ),

    ###########
    # Pragmas #
    ###########

    pragma_arg=Row(Opt(A.identifier, "=>") >> 0, A.expression),

    pragma=Row("pragma", A.identifier,
               Opt("(", List(A.pragma_arg, ","), ")") >> 1) ^ Pragma,
)
