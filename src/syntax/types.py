from syntax import ASTNode, Field, A
from combinators import Opt, List, Or, Row, _, Null, \
    EnumType, Enum


class DiscriminantSpec(ASTNode):
    fields = [
        Field("ids", repr=True),
        Field("type_expr", repr=True),
        Field("default_expr", repr=True)
    ]


class TypeDiscriminant(ASTNode):
    fields = [
        Field("opening_par"),
        Field("discr_specs", repr=True, opt=True),
        Field("closing_par")
    ]


class TypeDef(ASTNode):
    abstract = True


class EnumTypeDef(TypeDef):
    fields = [
        Field("opening_par"),
        Field("enum_literals", repr=True),
        Field("closing_par")
    ]


class DiscreteChoice(ASTNode):
    pass


class Variant(ASTNode):
    fields = [
        Field("when_kw"),
        Field("choice_list", repr=True),
        Field("components", repr=True)
    ]


class VariantPart(ASTNode):
    fields = [
        Field("discr_name", repr=True),
        Field("variant", repr=True)
    ]


class ComponentDecl(ASTNode):
    fields = [
        Field("ids", repr=True),
        Field("component_def", repr=True),
        Field("default_expr", repr=True),
        Field("aspects", repr=True)
    ]


class ComponentList(ASTNode):
    fields = [
        Field("components", repr=True),
        Field("variant_part", repr=True)
    ]


class RecordDef(ASTNode):
    fields = [
        Field("tk_start"),
        Field("components", repr=True, opt=True),
        Field("tk_end")
    ]


class RecordTypeDef(TypeDef):
    fields = [
        Field("abstract", repr=True),
        Field("tagged", repr=True),
        Field("limited", repr=True),
        Field("record_def", repr=True)
    ]


class RealTypeDef(TypeDef):
    abstract = True


class FullTypeDecl(ASTNode):
    fields = [
        Field("type_kw"),
        Field("type_id", repr=True),
        Field("discriminants", repr=True),
        Field("type_def", repr=True),
        Field("aspects", repr=True)
    ]


class FloatingPointDef(RealTypeDef):
    fields = [
        Field("digits_kw"),
        Field("num_digits", repr=True),
        Field("range", repr=True)
    ]


class OrdinaryFixedPointDef(RealTypeDef):
    fields = [
        Field("delta_kw"),
        Field("delta", repr=True),
        Field("range", repr=True)
    ]


class DecimalFixedPointDef(RealTypeDef):
    fields = [
        Field("delta_kw"),
        Field("delta", repr=True),
        Field("digits", repr=True),
        Field("range", repr=True)
    ]


class Constraint(ASTNode):
    abstract = True


class RangeConstraint(Constraint):
    fields = [Field("range", repr=True)]


class DigitsConstraint(Constraint):
    fields = [
        Field("digits", repr=True),
        Field("range", repr=True)
    ]


class DeltaConstraint(Constraint):
    fields = [
        Field("digits", repr=True),
        Field("range", repr=True)
    ]


class IndexConstraint(Constraint):
    fields = [Field("constraints", repr=True)]


class DiscriminantConstraint(Constraint):
    fields = [Field("constraints", repr=True)]


class DiscriminantAssociation(Constraint):
    fields = [
        Field("ids", repr=True),
        Field("expr", repr=True)
    ]


class DerivedTypeDef(TypeDef):
    fields = [
        Field("abstract", repr=True),
        Field("limited", repr=True),
        Field("synchronized", repr=True),
        Field("null_exclusion", repr=True),
        Field("name", repr=True),
        Field("constraint", repr=True),
        Field("interfaces", repr=True),
        Field("record_extension", repr=True),
        Field("has_private_part", repr=True)
    ]


class IncompleteTypeDef(TypeDef):
    fields = [
        Field("is_tagged", repr=True)
    ]


class PrivateTypeDef(TypeDef):
    fields = [
        Field("abstract", repr=True),
        Field("tagged", repr=True),
        Field("limited", repr=True),
        Field("private_kw")
    ]


class SignedIntTypeDef(TypeDef):
    fields = [
        Field("range", repr=True)
    ]


class ModIntTypeDef(TypeDef):
    fields = [
        Field("mod_kw"),
        Field("expr", repr=True)
    ]


class ArrayIndices(ASTNode):
    abstract = True


class UnconstrainedArrayIndices(ArrayIndices):
    fields = [
        Field("list", repr=True)
    ]


class ConstrainedArrayIndices(ArrayIndices):
    fields = [
        Field("list", repr=True)
    ]


class ComponentDef(ASTNode):
    fields = [
        Field("aliased", repr=True),
        Field("type_expr", repr=True)
    ]


class ArrayTypeDef(TypeDef):
    fields = [
        Field("array_kw"),
        Field("indices", repr=True),
        Field("stored_component", repr=True)
    ]


class InterfaceKind(EnumType):
    alternatives = ["limited", "task", "_protected", "synchronized"]


class InterfaceTypeDef(TypeDef):
    fields = [
        Field("interface_kind", repr=True),
        Field("interfaces", repr=True)
    ]


class SubtypeDecl(ASTNode):
    fields = [
        Field("tk_subtype"),
        Field("id", repr=True),
        Field("type_expr", repr=True),
        Field("aspects", repr=True)
    ]


class TaskDef(ASTNode):
    fields = [
        Field("items", repr=True),
        Field("private_items", repr=True),
        Field("end_id", repr=True)
    ]


class ProtectedDef(ASTNode):
    fields = [
        Field("public_ops", repr=True),
        Field("private_components", repr=True),
        Field("end_id", repr=True)
    ]


class TaskTypeDecl(ASTNode):
    fields = [
        Field("task_kw"),
        Field("task_type_name", repr=True),
        Field("discrs", repr=True),
        Field("aspects", repr=True),
        Field("interfaces", repr=True),
        Field("def", repr=True)
    ]


class ProtectedTypeDecl(ASTNode):
    fields = [
        Field("task_type_name", repr=True),
        Field("discrs", repr=True),
        Field("aspects", repr=True),
        Field("interfaces", repr=True),
        Field("def", repr=True)
    ]


class AccessDef(TypeDef):
    fields = [
        Field("not_null", repr=True),
        Field("access_expr", repr=True)
    ]


class FormalDiscreteTypeDef(TypeDef):
    fields = [Field("diamond")]


class NullComponentDecl(ASTNode):
    fields = []


A.add_rules(
    protected_type_decl=Row(
        _("protected"), _("type"), A.identifier, Opt(A.type_discriminant),
        A.aspect_specification,
        _("is"), Opt("new", List(A.static_name, sep="and"), "with") >> 1,
        A.protected_def
    ) ^ ProtectedTypeDecl,

    protected_op=Or(A.subprogram_decl, A.entry_decl, A.aspect_clause,
                    A.pragma),
    protected_el=Or(A.protected_op, A.component_decl),

    protected_def=Row(
        List(Row(A.protected_op, ";") >> 0, empty_valid=True),
        Opt("private", List(Row(A.protected_el, ";") >> 0,
                            empty_valid=True))
        >> 1,
        _("end"),
        Opt(A.identifier)
    ) ^ ProtectedDef,

    task_item=Or(A.entry_decl, A.aspect_clause, A.pragma),

    task_def=Row(
        List(Row(A.task_item, ";") >> 0, empty_valid=True),
        Opt("private", List(Row(A.task_item, ";") >> 0, empty_valid=True)) >> 1,
        _("end"),
        Opt(A.identifier)
    ) ^ TaskDef,

    task_type_decl=Row(
        "task", _("type"), A.identifier, Opt(A.type_discriminant),
        A.aspect_specification,
        _("is"), Opt("new", List(A.static_name, sep="and"), "with") >> 1,
        A.task_def
    ) ^ TaskTypeDecl,

    subtype_decl=Row(
        "subtype", A.identifier, _("is"), A.type_expression,
        A.aspect_specification
    ) ^ SubtypeDecl,

    interface_type_def=Row(
        Opt(Or(
            Enum("limited", InterfaceKind("limited")),
            Enum("task", InterfaceKind("task")),
            Enum("protected", InterfaceKind("_protected")),
            Enum("synchronized", InterfaceKind("synchronized")))),
        _("interface"),
        List(Row("and", A.static_name) >> 1, empty_valid=True)
    ) ^ InterfaceTypeDef,

    array_type_def=Row(
        "array",
        _("("),
        Or(
            List(Row(A.type_name, "range", "<>") >> 0, sep=",")
            ^ UnconstrainedArrayIndices,

            List(A.discrete_subtype_definition, sep=",")
            ^ ConstrainedArrayIndices
        ),
        _(")"), _("of"), A.component_def
    ) ^ ArrayTypeDef,

    discrete_subtype_definition=A.discrete_range | A.type_expression,

    signed_int_type_def=Row(A.range_spec) ^ SignedIntTypeDef,
    mod_int_type_def=Row("mod", A.sexpr_or_diamond) ^ ModIntTypeDef,

    derived_type_def=Row(
        Opt("abstract").as_bool(),
        Opt("limited").as_bool(),
        Opt("synchronized").as_bool(),
        _("new"),
        Opt("not", "null").as_bool(),
        A.type_expression,
        Opt(A.constraint),
        List(Row("and", A.static_name) >> 1, empty_valid=True),
        Opt("with", A.record_def) >> 1,
        Opt("with", "private").as_bool()
    ) ^ DerivedTypeDef,

    discriminant_association=Row(
        List(A.identifier, sep="|"), _("=>"), A.expression
    ),

    discriminant_constraint=Row(
        _("("), List(A.discriminant_association, sep=","), _(")")
    ) ^ DiscriminantConstraint,

    index_constraint=Row(
        _("("), List(A.discrete_subtype_definition, sep=","), _(")")
    ) ^ IndexConstraint,

    digits_constraint=Row(
        _("digits"), A.simple_expr, Opt(A.range_spec)
    ) ^ DigitsConstraint,

    delta_constraint=Row(
        _("delta"), A.simple_expr, Opt(A.range_spec)
    ) ^ DeltaConstraint,

    range_constraint=Row(A.range_spec) ^ RangeConstraint,

    constraint=Or(A.digits_constraint, A.delta_constraint,
                  A.range_constraint, A.index_constraint,
                  A.discriminant_constraint),

    discriminant_spec=Row(
        List(A.identifier, sep=","), _(":"), A.type_expression,
        A.default_expr
    ) ^ DiscriminantSpec,

    discr_spec_list=List(A.discriminant_spec, sep=";"),

    type_discriminant=Row(
        "(",
        Or(A.discr_spec_list, Row("<>", Null(A.discr_spec_list)) >> 1),
        ")"
    ) ^ TypeDiscriminant,

    enum_type_def=Row(
        "(", List(Or(A.identifier, A.char_literal), sep=","), ")"
    ) ^ EnumTypeDef,

    formal_discrete_type_def=Row(
        _("("), "<>", _(")")
    ) ^ FormalDiscreteTypeDef,

    record_def=Or(
        Row("record", A.component_list, _("end"), "record") ^ RecordDef,
        Row("null", Null(ComponentList), "record") ^ RecordDef
    ),

    range_spec=Row("range", A.discrete_range | A.name | A.diamond_expr) >> 1,

    real_type_def=Or(A.floating_point_def, A.decimal_fixed_point_def,
                     A.ordinary_fixed_point_def),

    sexpr_or_diamond=A.simple_expr | A.diamond_expr,

    ordinary_fixed_point_def=Row(
        "delta", A.sexpr_or_diamond, Opt(A.range_spec),
    ) ^ OrdinaryFixedPointDef,

    decimal_fixed_point_def=Row(
        "delta", A.sexpr_or_diamond, _("digits"),
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
        "when", A.choice_list, _("=>"), A.component_list
    ) ^ Variant,

    full_type_decl=Row(
        "type", A.identifier, Opt(A.type_discriminant),
        Or(
            Row("is", A.type_def) >> 1,

            Row(_("is"),
                Opt("abstract").as_bool(), Opt("tagged").as_bool(),
                Opt("limited").as_bool(), "private") ^ PrivateTypeDef,

            Row(Opt("is", "tagged").as_bool()) ^ IncompleteTypeDef,
        ),
        A.aspect_specification
    ) ^ FullTypeDecl,

    variant_part=Row(
        _("case"), A.identifier, _("is"),
        List(A.variant),
        _("end"), _("case"), _(";")
    ) ^ VariantPart,

    component_def=Row(
        Opt("aliased").as_bool(), A.type_expression) ^ ComponentDef,

    component_item=Or(
        Row(_("null")) ^ NullComponentDecl,
        A.component_decl,
        A.aspect_clause,
        A.pragma
    ),

    default_expr=Opt(":=", A.expression) >> 1,

    component_decl=Row(
        List(A.identifier, sep=","), _(":"), A.component_def,
        A.default_expr, A.aspect_specification
    ) ^ ComponentDecl,

    component_list=
        Row(List(Row(A.component_item, ";") >> 0, empty_valid=True),
            Opt(A.variant_part)) ^ ComponentList,
)
