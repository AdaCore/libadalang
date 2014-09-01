from ada_parser import  A
from parsers import Opt, List, Or, Row, _, Null, \
    EnumType, Enum, ASTNode, Field


class DiscriminantSpec(ASTNode):
    fields = [
        Field("ids"),
        Field("type_expr"),
        Field("default_expr")
    ]


class TypeDiscriminant(ASTNode):
    fields = [
        Field("opening_par", repr=False),
        Field("discr_specs"),
        Field("closing_par", repr=False)
    ]


class TypeDef(ASTNode):
    abstract = True


class EnumTypeDef(TypeDef):
    fields = [
        Field("opening_par", repr=False),
        Field("enum_literals"),
        Field("closing_par", repr=False)
    ]


class DiscreteChoice(ASTNode):
    pass


class Variant(ASTNode):
    fields = [
        Field("when_kw", repr=False),
        Field("choice_list"),
        Field("components")
    ]


class VariantPart(ASTNode):
    fields = [
        Field("discr_name"),
        Field("variant")
    ]


class ComponentDecl(ASTNode):
    fields = [
        Field("ids"),
        Field("component_def"),
        Field("default_expr"),
        Field("aspects")
    ]


class ComponentList(ASTNode):
    fields = [
        Field("components"),
        Field("variant_part")
    ]


class RecordDef(ASTNode):
    fields = [
        Field("tk_start", repr=False),
        Field("components"),
        Field("tk_end", repr=False)
    ]


class RecordTypeDef(TypeDef):
    fields = [
        Field("abstract"),
        Field("tagged"),
        Field("limited"),
        Field("record_def")
    ]


class RealTypeDef(TypeDef):
    abstract = True


class FullTypeDecl(ASTNode):
    fields = [
        Field("type_kw", repr=False),
        Field("type_id"),
        Field("discriminants"),
        Field("type_def"),
        Field("aspects")
    ]


class FloatingPointDef(RealTypeDef):
    fields = [
        Field("digits_kw", repr=False),
        Field("num_digits"),
        Field("range")
    ]


class OrdinaryFixedPointDef(RealTypeDef):
    fields = [
        Field("delta_kw", repr=False),
        Field("delta"),
        Field("range")
    ]


class DecimalFixedPointDef(RealTypeDef):
    fields = [
        Field("delta_kw", repr=False),
        Field("delta"),
        Field("digits"),
        Field("range")
    ]


class Constraint(ASTNode):
    abstract = True


class RangeConstraint(Constraint):
    fields = [Field("range")]


class DigitsConstraint(Constraint):
    fields = [
        Field("digits"),
        Field("range")
    ]


class DeltaConstraint(Constraint):
    fields = [
        Field("digits"),
        Field("range")
    ]


class IndexConstraint(Constraint):
    fields = [Field("constraints")]


class DiscriminantConstraint(Constraint):
    fields = [Field("constraints")]


class DiscriminantAssociation(Constraint):
    fields = [
        Field("ids"),
        Field("expr")
    ]


class DerivedTypeDef(TypeDef):
    fields = [
        Field("abstract"),
        Field("limited"),
        Field("synchronized"),
        Field("null_exclusion"),
        Field("name"),
        Field("constraint"),
        Field("interfaces"),
        Field("record_extension"),
        Field("has_private_part")
    ]


class IncompleteTypeDef(TypeDef):
    fields = [
        Field("is_tagged")
    ]


class PrivateTypeDef(TypeDef):
    fields = [
        Field("abstract"),
        Field("tagged"),
        Field("limited"),
        Field("private_kw", repr=False)
    ]


class SignedIntTypeDef(TypeDef):
    fields = [
        Field("range")
    ]


class ModIntTypeDef(TypeDef):
    fields = [
        Field("mod_kw", repr=False),
        Field("expr")
    ]


class ArrayIndices(ASTNode):
    abstract = True


class UnconstrainedArrayIndices(ArrayIndices):
    fields = [
        Field("list")
    ]


class ConstrainedArrayIndices(ArrayIndices):
    fields = [
        Field("list")
    ]


class ComponentDef(ASTNode):
    fields = [
        Field("aliased"),
        Field("type_expr")
    ]


class ArrayTypeDef(TypeDef):
    fields = [
        Field("array_kw", repr=False),
        Field("indices"),
        Field("stored_component")
    ]


class InterfaceKind(EnumType):
    alternatives = ["limited", "task", "_protected", "synchronized"]


class InterfaceTypeDef(TypeDef):
    fields = [
        Field("interface_kind"),
        Field("interfaces")
    ]


class SubtypeDecl(ASTNode):
    fields = [
        Field("tk_subtype", repr=False),
        Field("id"),
        Field("type_expr"),
        Field("aspects")
    ]


class TaskDef(ASTNode):
    fields = [
        Field("items"),
        Field("private_items"),
        Field("end_id")
    ]


class ProtectedDef(ASTNode):
    fields = [
        Field("public_ops"),
        Field("private_components"),
        Field("end_id")
    ]


class TaskTypeDecl(ASTNode):
    fields = [
        Field("task_kw", repr=False),
        Field("task_type_name"),
        Field("discrs"),
        Field("aspects"),
        Field("interfaces"),
        Field("def")
    ]


class ProtectedTypeDecl(ASTNode):
    fields = [
        Field("task_type_name"),
        Field("discrs"),
        Field("aspects"),
        Field("interfaces"),
        Field("def")
    ]


class AccessDef(TypeDef):
    fields = [
        Field("not_null"),
        Field("access_expr")
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
