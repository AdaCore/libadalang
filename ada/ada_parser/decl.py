from ada_parser import A
from parsers import Field, ASTNode, Opt, List, Or, Row, _, EnumType, Enum


class WithDecl(ASTNode):
    fields = [
        Field("is_limited"),
        Field("is_private"),
        Field("packages")
    ]


class UseDecl(ASTNode):
    abstract = True


class UsePkgDecl(UseDecl):
    fields = [
        Field("start_token", repr=False),
        Field("packages")
    ]


class UseTypDecl(UseDecl):
    fields = [
        Field("start_token", repr=False),
        Field("all"),
        Field("types")
    ]


class TypeExpression(ASTNode):
    """
    This type will be used as a base for what represents a type expression
    in the Ada syntax tree.
    """
    fields = [
        Field("null_exclusion"),
        Field("type_expr_variant")
    ]


class TypeExprVariant(ASTNode):
    abstract = True


class TypeRef(TypeExprVariant):
    fields = [
        Field("name"),
        Field("constraint")
    ]


class AccessExpression(TypeExprVariant):
    abstract = True


class SubprogramAccessExpression(AccessExpression):
    fields = [
        Field("access_token", repr=False),
        Field("is_protected", repr=False),
        Field("subp_spec")
    ]


class TypeAccessExpression(AccessExpression):
    fields = [
        Field("is_all"),
        Field("is_constant"),
        Field("subtype_name")
    ]


class ParameterProfile(ASTNode):
    fields = [
        Field("ids"),
        Field("is_aliased", repr=False),
        Field("mode"),
        Field("type_expr"),
        Field("_default")
    ]


class AspectSpecification(ASTNode):
    fields = [
        Field("aspect_assocs")
    ]


class SubprogramSpec(ASTNode):
    fields = [
        Field("tk_start", repr=False),
        Field("name"),
        Field("params"),
        Field("returns"),
    ]


class SubprogramDecl(ASTNode):
    fields = [
        Field("is_overriding"),
        Field("subp_spec"),
        Field("is_null"),
        Field("is_abstract"),
        Field("expression"),
        Field("renames"),
        Field("aspects", repr=False),
    ]


class Pragma(ASTNode):
    fields = [
        Field("pragma_kw", repr=False),
        Field("id"),
        Field("args"),
    ]


# #####################
# GRAMMAR DEFINITION #
######################

class InOut(EnumType):
    alternatives = ["in", "out", "inout"]


class AspectClause(ASTNode):
    abstract = True


class EnumRepClause(AspectClause):
    fields = [
        Field("type_name"),
        Field("aggregate")
    ]


class AttributeDefClause(AspectClause):
    fields = [
        Field("attribute_expr"),
        Field("expr")
    ]


class RecordRepComponent(ASTNode):
    fields = [
        Field("id"),
        Field("position"),
        Field("range")
    ]


class RecordRepClause(AspectClause):
    fields = [
        Field("component_name"),
        Field("at_expr"),
        Field("components")
    ]


class AtClause(AspectClause):
    fields = [
        Field("name"),
        Field("expr"),
    ]


class EntryDecl(ASTNode):
    fields = [
        Field("overriding"),
        Field("entry_id"),
        Field("family_type"),
        Field("params"),
        Field("aspects")
    ]


class TaskDecl(ASTNode):
    fields = [
        Field("task_name"),
        Field("aspects"),
        Field("def")
    ]


class ProtectedDecl(ASTNode):
    fields = [
        Field("protected_name"),
        Field("aspects"),
        Field("def")
    ]


class AspectAssoc(ASTNode):
    fields = [
        Field("id"),
        Field("expr")
    ]


class NumberDecl(ASTNode):
    fields = [
        Field("ids"),
        Field("expr")
    ]


class ObjectDecl(ASTNode):
    fields = [
        Field("ids"),
        Field("aliased"),
        Field("constant"),
        Field("inout"),
        Field("type"),
        Field("default_expr"),
        Field("renaming_clause"),
        Field("aspects")
    ]

    # properties = {
    #     "type": ChildNodeProperty("type")
    # }

    # env_action = AddToEnv("vars", "ids")


class PackageDecl(ASTNode):
    fields = [
        Field("name"),
        Field("aspects"),
        Field("decls"),
        Field("private_decls"),
    ]


class ExceptionDecl(ASTNode):
    fields = [
        Field("ids"),
        Field("renames"),
        Field("aspects")
    ]


class GenericInstantiation(ASTNode):
    fields = [
        Field("type_token", repr=False),
        Field("name"),
        Field("generic_entity_name"),
        Field("parameters"),
        Field("aspects")
    ]


class RenamingClause(ASTNode):
    fields = [Field("renamed_object")]


class PackageRenamingDecl(ASTNode):
    fields = [
        Field("name"),
        Field("renames"),
        Field("aspects"),
    ]


class GenericRenamingDecl(ASTNode):
    fields = [
        Field("name"),
        Field("renames"),
        Field("aspects")
    ]


class FormalSubpDecl(ASTNode):
    fields = [
        Field("subp_spec"),
        Field("is_abstract"),
        Field("default_value")
    ]


class Overriding(EnumType):
    alternatives = ["overriding", "not_overriding", "unspecified"]


class GenericSubprogramDecl(ASTNode):
    fields = [
        Field("formal_part"),
        Field("subp_spec")
    ]


class GenericPackageDecl(ASTNode):
    fields = [
        Field("formal_part"),
        Field("package_decl")
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
