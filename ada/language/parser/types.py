from langkit.compiled_types import Field, abstract, EnumType
from langkit.envs import EnvSpec
from langkit.expressions import Property, Self
from langkit.parsers import Opt, List, Or, Row, Null, Enum

from language.parser import A, AdaNode


class DiscriminantSpec(AdaNode):
    ids = Field()
    type_expr = Field()
    default_expr = Field()


class TypeDiscriminant(AdaNode):
    discr_specs = Field()


@abstract
class TypeDef(AdaNode):
    pass


class EnumTypeDef(TypeDef):
    enum_literals = Field()


class Variant(AdaNode):
    choice_list = Field()
    components = Field()


class VariantPart(AdaNode):
    discr_name = Field()
    variant = Field()


class ComponentDecl(AdaNode):
    ids = Field()
    component_def = Field()
    default_expr = Field()
    aspects = Field()


class ComponentList(AdaNode):
    components = Field()
    variant_part = Field()


class RecordDef(AdaNode):
    components = Field()


class RecordTypeDef(TypeDef):
    abstract = Field()
    tagged = Field()
    limited = Field()
    record_def = Field()


@abstract
class RealTypeDef(TypeDef):
    pass


class FullTypeDecl(AdaNode):
    type_id = Field()
    discriminants = Field()
    type_def = Field()
    aspects = Field()


class FloatingPointDef(RealTypeDef):
    num_digits = Field()
    range = Field()


class OrdinaryFixedPointDef(RealTypeDef):
    delta = Field()
    range = Field()


class DecimalFixedPointDef(RealTypeDef):
    delta = Field()
    digits = Field()
    range = Field()


@abstract
class Constraint(AdaNode):
    pass


class RangeConstraint(Constraint):
    range = Field()


class DigitsConstraint(Constraint):
    digits = Field()
    range = Field()


class DeltaConstraint(Constraint):
    digits = Field()
    range = Field()


class IndexConstraint(Constraint):
    constraints = Field()


class DiscriminantConstraint(Constraint):
    constraints = Field()


class DiscriminantAssociation(Constraint):
    ids = Field()
    expr = Field()


class DerivedTypeDef(TypeDef):
    abstract = Field()
    limited = Field()
    synchronized = Field()
    null_exclusion = Field()
    name = Field()
    constraint = Field()
    interfaces = Field()
    record_extension = Field()
    has_private_part = Field()

    env_spec = EnvSpec(add_env=True)


class IncompleteTypeDef(TypeDef):
    is_tagged = Field()


class PrivateTypeDef(TypeDef):
    abstract = Field()
    tagged = Field()
    limited = Field()


class SignedIntTypeDef(TypeDef):
    range = Field()


class ModIntTypeDef(TypeDef):
    expr = Field()


@abstract
class ArrayIndices(AdaNode):
    pass


class UnconstrainedArrayIndices(ArrayIndices):
    list = Field()


class ConstrainedArrayIndices(ArrayIndices):
    list = Field()


class ComponentDef(AdaNode):
    aliased = Field()
    type_expr = Field()


class ArrayTypeDef(TypeDef):
    indices = Field()
    stored_component = Field()


class InterfaceKind(EnumType):
    alternatives = ["limited", "task", "protected", "synchronized"]
    suffix = 'interface'


class InterfaceTypeDef(TypeDef):
    interface_kind = Field()
    interfaces = Field()


class SubtypeDecl(AdaNode):
    # Fields
    id = Field()
    type_expr = Field()
    aspects = Field()

    # Properties
    name = Property(Self.id, doc='Name for the declared subtype')

    env_spec = EnvSpec(add_to_env=(Self.name, Self))


class TaskDef(AdaNode):
    items = Field()
    private_items = Field()
    end_id = Field()


class ProtectedDef(AdaNode):
    public_ops = Field()
    private_components = Field()
    end_id = Field()


class TaskTypeDecl(AdaNode):
    task_type_name = Field()
    discrs = Field()
    aspects = Field()
    interfaces = Field()
    definition = Field()


class ProtectedTypeDecl(AdaNode):
    task_type_name = Field()
    discrs = Field()
    aspects = Field()
    interfaces = Field()
    definition = Field()


class AccessDef(TypeDef):
    not_null = Field()
    access_expr = Field()


class FormalDiscreteTypeDef(TypeDef):
    pass


class NullComponentDecl(AdaNode):
    pass


A.add_rules(
    protected_type_decl=Row(
        "protected", "type", A.identifier, Opt(A.type_discriminant),
        A.aspect_specification,
        "is", Opt("new", List(A.static_name, sep="and"), "with")[1],
        A.protected_def
    ) ^ ProtectedTypeDecl,

    protected_op=Or(A.subprogram_decl, A.entry_decl, A.aspect_clause,
                    A.pragma),
    protected_el=Or(A.protected_op, A.component_decl),

    protected_def=Row(
        List(Row(A.protected_op, ";")[0], empty_valid=True),
        Opt(
            "private",
            List(Row(A.protected_el, ";")[0], empty_valid=True)
        )[1],
        "end",
        Opt(A.identifier)
    ) ^ ProtectedDef,

    task_item=Or(A.entry_decl, A.aspect_clause, A.pragma),

    task_def=Row(
        List(Row(A.task_item, ";")[0], empty_valid=True),
        Opt(
            "private", List(Row(A.task_item, ";")[0], empty_valid=True)
        )[1],
        "end",
        Opt(A.identifier)
    ) ^ TaskDef,

    task_type_decl=Row(
        "task", "type", A.identifier, Opt(A.type_discriminant),
        A.aspect_specification,
        "is", Opt("new", List(A.static_name, sep="and"), "with")[1],
        A.task_def
    ) ^ TaskTypeDecl,

    subtype_decl=Row(
        "subtype", A.identifier, "is", A.type_expression,
        A.aspect_specification
    ) ^ SubtypeDecl,

    interface_type_def=Row(
        Opt(Or(
            Enum("limited", InterfaceKind("limited")),
            Enum("task", InterfaceKind("task")),
            Enum("protected", InterfaceKind("protected")),
            Enum("synchronized", InterfaceKind("synchronized")))),
        "interface",
        List(Row("and", A.static_name)[1], empty_valid=True)
    ) ^ InterfaceTypeDef,

    array_type_def=Row(
        "array",
        "(",
        Or(
            List(Row(A.type_name, "range", "<>")[0], sep=",")
            ^ UnconstrainedArrayIndices,

            List(A.discrete_subtype_definition, sep=",")
            ^ ConstrainedArrayIndices
        ),
        ")", "of", A.component_def
    ) ^ ArrayTypeDef,

    discrete_subtype_definition=A.discrete_range | A.type_expression,

    signed_int_type_def=Row(A.range_spec) ^ SignedIntTypeDef,
    mod_int_type_def=Row("mod", A.sexpr_or_diamond) ^ ModIntTypeDef,

    derived_type_def=Row(
        Opt("abstract").as_bool(),
        Opt("limited").as_bool(),
        Opt("synchronized").as_bool(),
        "new",
        Opt("not", "null").as_bool(),
        A.type_expression,
        Opt(A.constraint),
        List(Row("and", A.static_name)[1], empty_valid=True),
        Opt("with", A.record_def)[1],
        Opt("with", "private").as_bool()
    ) ^ DerivedTypeDef,

    discriminant_association=Row(
        List(A.identifier, sep="|"), "=>", A.expression
    ) ^ DiscriminantAssociation,

    discriminant_constraint=Row(
        "(", List(A.discriminant_association, sep=","), ")"
    ) ^ DiscriminantConstraint,

    index_constraint=Row(
        "(", List(A.discrete_subtype_definition, sep=","), ")"
    ) ^ IndexConstraint,

    digits_constraint=Row(
        "digits", A.simple_expr, Opt(A.range_spec)
    ) ^ DigitsConstraint,

    delta_constraint=Row(
        "delta", A.simple_expr, Opt(A.range_spec)
    ) ^ DeltaConstraint,

    range_constraint=Row(A.range_spec) ^ RangeConstraint,

    constraint=Or(A.digits_constraint, A.delta_constraint,
                  A.range_constraint, A.index_constraint,
                  A.discriminant_constraint),

    discriminant_spec=Row(
        List(A.identifier, sep=","), ":", A.type_expression,
        A.default_expr
    ) ^ DiscriminantSpec,

    discr_spec_list=List(A.discriminant_spec, sep=";"),

    type_discriminant=Row(
        "(",
        Or(A.discr_spec_list, Row("<>", Null(A.discr_spec_list))[1]),
        ")"
    ) ^ TypeDiscriminant,

    enum_type_def=Row(
        "(", List(Or(A.enum_identifier, A.char_literal), sep=","), ")"
    ) ^ EnumTypeDef,

    formal_discrete_type_def=Row(
        "(", "<>", ")"
    ) ^ FormalDiscreteTypeDef,

    record_def=Or(
        Row("record", A.component_list, "end", "record") ^ RecordDef,
        Row("null", Null(ComponentList), "record") ^ RecordDef
    ),

    range_spec=Row("range", A.discrete_range | A.name | A.diamond_expr)[1],

    real_type_def=Or(A.floating_point_def, A.decimal_fixed_point_def,
                     A.ordinary_fixed_point_def),

    sexpr_or_diamond=A.simple_expr | A.diamond_expr,

    ordinary_fixed_point_def=Row(
        "delta", A.sexpr_or_diamond, Opt(A.range_spec),
    ) ^ OrdinaryFixedPointDef,

    decimal_fixed_point_def=Row(
        "delta", A.sexpr_or_diamond, "digits",
        A.sexpr_or_diamond, Opt(A.range_spec)
    ) ^ DecimalFixedPointDef,

    floating_point_def=Row(
        "digits", A.sexpr_or_diamond, Opt(A.range_spec)
    ) ^ FloatingPointDef,

    record_type_def=Row(
        Opt("abstract").as_bool(),
        Opt("tagged").as_bool(),
        Opt("limited").as_bool(),
        A.record_def
    ) ^ RecordTypeDef,

    access_def=Row(
        Opt("not", "null").as_bool(),
        A.access_expression
    ) ^ AccessDef,

    type_def=Or(A.enum_type_def, A.record_type_def, A.real_type_def,
                A.derived_type_def, A.signed_int_type_def,
                A.mod_int_type_def, A.array_type_def, A.interface_type_def,
                A.access_def, A.formal_discrete_type_def),

    variant=Row(
        "when", A.choice_list, "=>", A.component_list
    ) ^ Variant,

    full_type_decl=Row(
        "type", A.identifier, Opt(A.type_discriminant),
        Or(
            Row("is", A.type_def)[1],

            Row("is",
                Opt("abstract").as_bool(), Opt("tagged").as_bool(),
                Opt("limited").as_bool(), "private") ^ PrivateTypeDef,

            Row(Opt("is", "tagged").as_bool()) ^ IncompleteTypeDef,
        ),
        A.aspect_specification
    ) ^ FullTypeDecl,

    variant_part=Row(
        "case", A.identifier, "is",
        List(A.variant),
        "end", "case", ";"
    ) ^ VariantPart,

    component_def=Row(
        Opt("aliased").as_bool(), A.type_expression) ^ ComponentDef,

    component_item=Or(
        Row("null") ^ NullComponentDecl,
        A.component_decl,
        A.aspect_clause,
        A.pragma
    ),

    default_expr=Opt(":=", A.expression)[1],

    component_decl=Row(
        List(A.identifier, sep=","), ":", A.component_def,
        A.default_expr, A.aspect_specification
    ) ^ ComponentDecl,

    component_list=Row(
        List(Row(A.component_item, ";")[0], empty_valid=True),
        Opt(A.variant_part)
    ) ^ ComponentList,
)
