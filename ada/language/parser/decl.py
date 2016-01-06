from langkit.compiled_types import EnumType, Field, Struct, Token, abstract
from langkit.expressions import New, Property, Self
from langkit.parsers import Enum, List, Opt, Or, Row, _

from language.parser import A, AdaNode
from language.parser.exprs import Identifier


class WithDecl(AdaNode):
    is_limited = Field()
    is_private = Field()
    packages = Field()


@abstract
class UseDecl(AdaNode):
    pass


class UsePkgDecl(UseDecl):
    packages = Field()


class UseTypDecl(UseDecl):
    all = Field()
    types = Field()


class TypeExpression(AdaNode):
    """
    This type will be used as a base for what represents a type expression
    in the Ada syntax tree.
    """
    null_exclusion = Field()
    type_expr_variant = Field()


@abstract
class TypeExprVariant(AdaNode):
    pass


class TypeRef(TypeExprVariant):
    name = Field()
    constraint = Field()


@abstract
class AccessExpression(TypeExprVariant):
    pass


class SubprogramAccessExpression(AccessExpression):
    is_protected = Field(repr=False)
    subp_spec = Field()


class TypeAccessExpression(AccessExpression):
    is_all = Field()
    is_constant = Field()
    subtype_name = Field()


class ParameterProfile(AdaNode):
    ids = Field()
    is_aliased = Field(repr=False)
    mode = Field()
    type_expr = Field()
    default = Field()


class AspectSpecification(AdaNode):
    aspect_assocs = Field()


class SingleParameter(Struct):
    name = Field(type=Identifier)
    type_expr = Field(type=TypeExpression)


class SubprogramSpec(AdaNode):
    name = Field()
    params = Field()
    returns = Field()

    typed_param_list = Property(
        Self.params.mapcat(
            lambda profile: profile.ids.map(
                lambda id: New(SingleParameter,
                               name=id,
                               type_expr=profile.type_expr),
            ),
        ),
        doc='Collection of couples (identifier, type expression) for all'
            ' parameters'
    )


class SubprogramDecl(AdaNode):
    is_overriding = Field()
    subp_spec = Field()
    is_null = Field()
    is_abstract = Field()
    expression = Field()
    renames = Field()
    aspects = Field(repr=False)


class Pragma(AdaNode):
    id = Field()
    args = Field()


class PragmaArgument(AdaNode):
    id = Field()
    expr = Field()


######################
# GRAMMAR DEFINITION #
######################

class InOut(EnumType):
    alternatives = ["in", "out", "inout"]
    suffix = 'way'


@abstract
class AspectClause(AdaNode):
    pass


class EnumRepClause(AspectClause):
    type_name = Field()
    aggregate = Field()


class AttributeDefClause(AspectClause):
    attribute_expr = Field()
    expr = Field()


class RecordRepComponent(AdaNode):
    id = Field()
    position = Field()
    range = Field()


class RecordRepClause(AspectClause):
    component_name = Field()
    at_expr = Field()
    components = Field()


class AtClause(AspectClause):
    name = Field()
    expr = Field()


class EntryDecl(AdaNode):
    overriding = Field()
    entry_id = Field()
    family_type = Field()
    params = Field()
    aspects = Field()


class TaskDecl(AdaNode):
    task_name = Field()
    aspects = Field()
    definition = Field()


class ProtectedDecl(AdaNode):
    protected_name = Field()
    aspects = Field()
    definition = Field()


class AspectAssoc(AdaNode):
    id = Field()
    expr = Field()


class NumberDecl(AdaNode):
    ids = Field()
    expr = Field()


class ObjectDecl(AdaNode):
    ids = Field()
    aliased = Field()
    constant = Field()
    inout = Field()
    type = Field()
    default_expr = Field()
    renaming_clause = Field()
    aspects = Field()

    # Incoming properties will read::
    #     properties = {
    #         "type": ChildNodeProperty("type")
    #     }
    #
    #     env_action = AddToEnv("vars", "ids")


class PrivatePart(AdaNode):
    decls = Field()


class PackageDecl(AdaNode):
    name = Field()
    aspects = Field()
    decls = Field()
    private_part = Field()
    end_id = Field()


class ExceptionDecl(AdaNode):
    ids = Field()
    renames = Field()
    aspects = Field()


class GenericInstantiation(AdaNode):
    name = Field()
    generic_entity_name = Field()
    parameters = Field()
    aspects = Field()


class RenamingClause(AdaNode):
    renamed_object = Field()


class PackageRenamingDecl(AdaNode):
    name = Field()
    renames = Field()
    aspects = Field()


class GenericRenamingDecl(AdaNode):
    name = Field()
    renames = Field()
    aspects = Field()


class FormalSubpDecl(AdaNode):
    subp_spec = Field()
    is_abstract = Field()
    default_value = Field()


class Overriding(EnumType):
    alternatives = ["overriding", "not_overriding", "unspecified"]
    suffix = 'kind'


class GenericSubprogramDecl(AdaNode):
    formal_part = Field()
    subp_spec = Field()


class GenericPackageDecl(AdaNode):
    formal_part = Field()
    package_decl = Field()


A.add_rules(
    generic_decl=Or(
        Row(A.generic_formal_part, A.subprogram_spec) ^ GenericSubprogramDecl,
        Row(A.generic_formal_part, A.package_decl) ^ GenericPackageDecl
    ),

    generic_formal_part=Row(
        "generic", List(Row(A.generic_formal_decl | A.use_decl, ";")[0],
                        empty_valid=True)
    )[1],

    generic_formal_decl=Or(
        A.pragma,
        A.object_decl,
        A.full_type_decl,
        A.formal_subp_decl,
        Row("with", A.generic_instantiation)[1]
    ),

    formal_subp_decl=Row(
        "with",
        A.subprogram_spec,
        _(Opt("is")),
        Opt("abstract").as_bool(),
        Opt(Or(A.diamond_expr, A.static_name, A.null_literal))
    ) ^ FormalSubpDecl,

    renaming_clause=Row("renames", A.name) ^ RenamingClause,

    generic_renaming_decl=Row(
        "generic", _(Or("package", "function", "procedure")), A.static_name,
        "renames", A.static_name, A.aspect_specification
    ) ^ GenericRenamingDecl,

    generic_instantiation=Row(
        _(Or("package", "function", "procedure")), A.static_name, "is",
        "new", A.static_name,
        Opt("(", A.call_suffix, ")")[1],
        A.aspect_specification
    ) ^ GenericInstantiation,

    exception_decl=Row(
        A.id_list, ":", "exception",
        Opt(A.renaming_clause), A.aspect_specification
    ) ^ ExceptionDecl,

    basic_decls=List(Row(A.basic_decl, ";")[0], empty_valid=True),

    package_renaming_decl=Row(
        "package", A.static_name, A.renaming_clause, A.aspect_specification
    ) ^ PackageRenamingDecl,

    package_decl=Row(
        "package", A.static_name, A.aspect_specification, "is",
        A.basic_decls,
        Opt("private", A.basic_decls ^ PrivatePart)[1],
        "end", Opt(A.static_name)
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
        A.sub_object_decl,
        A.task_decl,
        A.protected_decl,
        A.number_decl
    ),

    sub_object_decl=Row(
        A.id_list,  ":",
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
        A.id_list, ":", "constant", ":=",
        A.simple_expr
    ) ^ NumberDecl,

    aspect_assoc=Row(
        A.name, Opt("=>", A.expression)[1]
    ) ^ AspectAssoc,

    aspect_specification=Opt(
        Row(
            "with",
            List(A.aspect_assoc, sep=",")
        ) ^ AspectSpecification
    ),

    protected_decl=Row(
        "protected", A.identifier, A.aspect_specification,
        "is", A.protected_def
    ) ^ ProtectedDecl,

    task_decl=Row("task", A.identifier, A.aspect_specification,
                  Opt("is", A.task_def)[1]) ^ TaskDecl,

    overriding_indicator=Or(
        Enum("overriding", Overriding("overriding")),
        Enum(Row("not", "overriding"), Overriding("not_overriding")),
        Enum(None, Overriding("unspecified"))
    ),

    entry_decl=Row(
        A.overriding_indicator,
        "entry",
        A.identifier,
        Opt("(", A.type_ref, ")")[1],
        Opt(A.parameter_profiles),
        A.aspect_specification
    ) ^ EntryDecl,


    rep_component_clause=Row(
        A.identifier, "at", A.simple_expr, A.range_spec
    ) ^ RecordRepComponent,

    aspect_clause=Or(
        Row("for", A.name, "use", A.expression) ^ AttributeDefClause,

        Row("for", A.static_name, "use", A.aggregate) ^ EnumRepClause,

        Row("for", A.static_name, "use", "record",
            Opt("at", "mod", A.simple_expr)[2],
            List(Row(A.rep_component_clause, ";")[0], empty_valid=True),
            "end", "record") ^ RecordRepClause,

        Row("for", A.direct_name, "use", "at", A.expression) ^ AtClause
    ),

    parameter_profile=Row(
        List(A.identifier, sep=","),
        ":",
        Opt("aliased").as_bool(),
        Opt(A.in_out),
        A.type_expression,
        Opt(":=", A.expression)[1],
    ) ^ ParameterProfile,

    parameter_profiles=Row("(", List(A.parameter_profile, sep=";"), ")")[1],

    subprogram_spec=Row(
        _(Or("procedure", "function")),
        Opt(A.name),
        Opt(
            Row(
                "(",
                List(A.parameter_profile, sep=";"),
                Opt(")").error()
            )[1]
        ),
        Opt("return", A.type_expression)[1]
    ) ^ SubprogramSpec,

    test_spec=List(A.subprogram_spec, sep=";"),

    subprogram_decl=Row(
        A.overriding_indicator,
        A.subprogram_spec,
        Opt("is", "null").as_bool(),
        Opt("is", "abstract").as_bool(),
        Opt("is", A.expression)[1],
        Opt(A.renaming_clause),
        A.aspect_specification,
    ) ^ SubprogramDecl,

    type_access_expression=Row(
        "access", Opt("all").as_bool(), Opt("constant").as_bool(), A.name
    ) ^ TypeAccessExpression,

    with_decl=Row(
        Opt("limited").as_bool(), Opt("private").as_bool(),
        "with", List(A.static_name, sep=",")
    ) ^ WithDecl,

    context_item=Or(A.with_decl, A.use_decl, A.pragma),

    use_decl=Or(A.use_package_decl, A.use_type_decl),

    use_package_decl=Row("use", List(A.static_name, sep=",")) ^ UsePkgDecl,

    use_type_decl=Row("use", Opt("all").as_bool(), "type",
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

    pragma_arg=Row(
        Opt(A.identifier, "=>")[0], A.expression
    ) ^ PragmaArgument,

    pragma=Row("pragma", A.identifier,
               Opt("(", List(A.pragma_arg, ","), ")")[1]) ^ Pragma,
)
