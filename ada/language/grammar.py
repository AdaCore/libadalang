from __future__ import absolute_import

from langkit.lexer import LexerToken
from language.ast import *
from language.lexer import Token

# This import is after the language.ast import, because we want to be sure
# no class from langkit.expressions are shadowing the parser combinators.
from langkit.parsers import Grammar, Row, Enum, _, Null, Tok, Opt, List, Or

ada_grammar = Grammar(main_rule_name='compilation')
A = ada_grammar


def package_decl_factory():
    """
    Factory for creating a grammar rule that parses package declarations. Used
    to be able to generate both PackageDecl and BasePackageDecl instances.

    :rtype: Row
    """
    return Row(
        "package", A.static_name, A.aspect_spec, "is",
        A.basic_decls ^ PublicPart,
        Opt("private", A.basic_decls ^ PrivatePart)[1],
        "end", Opt(A.static_name)
    )


def subprogram_decl(variant_part, dest_class):
    """
    Factory for subprogram declarations grammar rules.

    :param Parser variant_part: The parser for the variant part of the
        subprogram declaration.
    :param dest_class: The destination AdaNode subclass to use for the result.
    :rtype: Transform
    """
    return Row(
        A.overriding_indicator,
        A.subprogram_spec,
        variant_part,
        A.aspect_spec,
    ) ^ dest_class


def generic_instantiation(keyword, dest_class):
    """
    Factory for generic instantiations grammar rules.

    :param str keyword: The variant keyword that will initiate the generic
        instantiation rule.
    :param dest_class: The destination AdaNode subclass to use for the result.
    :rtype: Transform
    """
    return Row(
        keyword, A.static_name, "is",
        "new", A.static_name,
        Opt("(", A.call_suffix, ")")[1],
        A.aspect_spec
    ) ^ dest_class

A.add_rules(
    protected_type_decl=Row(
        "protected", "type", A.identifier, Opt(A.discriminant_part),
        A.aspect_spec,
        "is", Opt("new", List(A.static_name, sep="and"), "with")[1],
        A.protected_def
    ) ^ ProtectedTypeDecl,

    protected_op=Or(A.subprogram_decl, A.entry_decl, A.aspect_clause,
                    A.pragma),
    protected_el=Or(A.protected_op, A.component_decl),

    protected_def=Row(
        List(Row(A.protected_op, ";")[0], empty_valid=True) ^ PublicPart,
        Opt(
            "private",
            List(Row(A.protected_el, ";")[0], empty_valid=True) ^ PrivatePart
        )[1],
        "end",
        Opt(A.identifier)
    ) ^ ProtectedDef,

    task_item=Or(A.entry_decl, A.aspect_clause, A.pragma),

    task_def=Row(
        "is",
        Opt("new", List(A.static_name, sep="and"), "with")[1],
        List(Row(A.task_item, ";")[0], empty_valid=True) ^ PublicPart,
        Opt(
            "private",
            List(Row(A.task_item, ";")[0], empty_valid=True) ^ PrivatePart
        )[1],
        "end",
        Opt(A.identifier)
    ) ^ TaskDef,

    task_type_decl=Row(
        "task", "type", A.identifier, Opt(A.discriminant_part),
        A.aspect_spec,
        Opt(A.task_def)
    ) ^ TaskTypeDecl,

    subtype_decl=Row(
        "subtype", A.identifier, "is", A.subtype_indication,
        A.aspect_spec
    ) ^ SubtypeDecl,

    interface_type_def=Row(
        Opt(Or(
            Row("limited") ^ InterfaceKind.alt_limited,
            Row("task") ^ InterfaceKind.alt_task,
            Row("protected") ^ InterfaceKind.alt_protected,
            Row("synchronized") ^ InterfaceKind.alt_synchronized)),
        "interface",
        List(Row("and", A.static_name)[1], empty_valid=True)
    ) ^ InterfaceTypeDef,

    array_type_def=Row(
        "array",
        "(",
        Or(
            List(Row(A.static_name, "range", "<>")[0], sep=",")
            ^ UnconstrainedArrayIndices,

            List(A.discrete_subtype_definition, sep=",")
            ^ ConstrainedArrayIndices
        ),
        ")", "of", A.component_def
    ) ^ ArrayTypeDef,

    discrete_subtype_definition=A.discrete_range | A.subtype_indication,

    signed_int_type_def=Row(A.range_spec) ^ SignedIntTypeDef,
    mod_int_type_def=Row("mod", A.sexpr_or_box) ^ ModIntTypeDef,

    derived_type_def=Row(
        Opt("abstract").as_bool(Abstract),
        Opt("limited").as_bool(Limited),
        Opt("synchronized").as_bool(Synchronized),
        "new",
        A.subtype_indication,
        List(Row("and", A.static_name)[1], empty_valid=True),
        Opt("with", A.record_def)[1],
        Opt("with", "private").as_bool(WithPrivate)
    ) ^ DerivedTypeDef,

    discriminant_assoc=Row(
        Opt(List(A.identifier, sep="|"), "=>")[0],
        A.expr
    ) ^ DiscriminantAssoc,

    discriminant_constraint=Row(
        "(", List(A.discriminant_assoc, sep=","), ")"
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
        List(A.identifier, sep=","), ":", A.type_expr,
        A.default_expr
    ) ^ DiscriminantSpec,

    discr_spec_list=List(A.discriminant_spec, sep=";"),

    discriminant_part=Or(
        Row("(", A.discr_spec_list, ")") ^ KnownDiscriminantPart,
        Row("(", "<>", ")") ^ UnknownDiscriminantPart,
    ),

    enum_literal_decl=(A.identifier | A.char_literal) ^ EnumLiteralDecl,

    formal_discrete_type_def=Row(
        "(", "<>", ")"
    ) ^ FormalDiscreteTypeDef,

    record_def=Or(
        Row("record", A.component_list, "end", "record") ^ RecordDef,
        Row("null", "record", Null(ComponentList)) ^ NullRecordDef
    ),

    range_spec=Row("range", A.discrete_range | A.name | A.box_expr)[1],

    real_type_def=Or(A.floating_point_def, A.decimal_fixed_point_def,
                     A.ordinary_fixed_point_def),

    sexpr_or_box=A.simple_expr | A.box_expr,

    ordinary_fixed_point_def=Row(
        "delta", A.sexpr_or_box, Opt(A.range_spec),
    ) ^ OrdinaryFixedPointDef,

    decimal_fixed_point_def=Row(
        "delta", A.sexpr_or_box, "digits",
        A.sexpr_or_box, Opt(A.range_spec)
    ) ^ DecimalFixedPointDef,

    floating_point_def=Row(
        "digits", A.sexpr_or_box, Opt(A.range_spec)
    ) ^ FloatingPointDef,

    record_type_def=Row(
        Opt("abstract").as_bool(Abstract),
        Opt("tagged").as_bool(Tagged),
        Opt("limited").as_bool(Limited),
        A.record_def
    ) ^ RecordTypeDef,

    access_def=Or(
        Row(
            Opt("not", "null").as_bool(NotNull),
            "access",
            Opt("protected").as_bool(Protected),
            A.subprogram_spec
        ) ^ AccessToSubprogramDef,
        Row(
            Opt("not", "null").as_bool(NotNull),
            "access",
            Opt("all").as_bool(All),
            Opt("constant").as_bool(Constant),
            A.subtype_name,
            Opt(A.constraint),
        ) ^ TypeAccessDef
    ),

    type_def=Or(A.record_type_def, A.real_type_def,
                A.derived_type_def, A.signed_int_type_def,
                A.mod_int_type_def, A.array_type_def, A.interface_type_def,
                A.access_def, A.formal_discrete_type_def),

    variant=Row(
        "when", A.choice_list, "=>", A.component_list
    ) ^ Variant,

    anonymous_type_decl=Row(
        Null(A.identifier), Null(A.discriminant_part),
        Or(A.array_type_def, A.access_def),
        A.aspect_spec
    ) ^ AnonymousTypeDecl,

    type_decl=Or(
        Row(
            "type", A.identifier, "is",
            "(", List(A.enum_literal_decl, sep=","), ")",
            A.aspect_spec
        ) ^ EnumTypeDecl,
        Row(
            "type", A.identifier, Opt(A.discriminant_part),
            Or(
                Row("is", A.type_def)[1],

                Row("is",
                    Opt("abstract").as_bool(Abstract),
                    Opt("tagged").as_bool(Tagged),
                    Opt("limited").as_bool(Limited),
                    "private") ^ PrivateTypeDef,

                Row(
                    Opt("is", "tagged").as_bool(Tagged)
                ) ^ IncompleteTypeDef,
            ),
            A.aspect_spec
        ) ^ TypeDecl
    ),

    variant_part=Row(
        "case", A.identifier, "is",
        List(A.variant),
        "end", "case", ";"
    ) ^ VariantPart,

    component_def=Row(
        Opt("aliased").as_bool(Aliased),
        A.type_expr
    ) ^ ComponentDef,

    component_item=Or(
        Row("null") ^ NullComponentDecl,
        A.component_decl,
        A.aspect_clause,
        A.pragma
    ),

    default_expr=Opt(":=", A.expr)[1],

    component_decl=Row(
        List(A.identifier, sep=","), ":", A.component_def,
        A.default_expr, A.aspect_spec
    ) ^ ComponentDecl,

    component_list=Row(
        List(Row(A.component_item, ";")[0], empty_valid=True),
        Opt(A.variant_part)
    ) ^ ComponentList,

    generic_decl=Or(
        Row(A.generic_formal_part, A.subprogram_spec,
            A.aspect_spec) ^ GenericSubprogramDecl,
        Row(A.generic_formal_part, A.base_package_decl) ^ GenericPackageDecl
    ),

    generic_formal_part=Row(
        "generic", List(Row(A.generic_formal_decl | A.use_clause, ";")[0],
                        empty_valid=True)
    )[1],

    generic_formal_decl=Or(
        A.pragma,
        A.object_decl,
        A.type_decl,
        A.formal_subp_decl,
        Row("with", A.generic_instantiation)[1]
    ),

    formal_subp_decl=Row(
        "with",
        A.subprogram_spec,
        _(Opt("is")),
        Opt("abstract").as_bool(Abstract),
        Opt(Or(A.box_expr, A.name, A.null_literal)),
        A.aspect_spec
    ) ^ FormalSubpDecl,

    renaming_clause=Row("renames", A.name) ^ RenamingClause,

    generic_renaming_decl=Row(
        "generic", _(Or("package", "function", "procedure")), A.static_name,
        "renames", A.static_name, A.aspect_spec
    ) ^ GenericRenamingDecl,

    generic_instantiation=Or(
        generic_instantiation("package", GenericPackageInstantiation),
        generic_instantiation("procedure", GenericProcedureInstantiation),
        generic_instantiation("function", GenericFunctionInstantiation),
    ),

    exception_decl=Row(
        A.id_list, ":", "exception",
        Opt(A.renaming_clause), A.aspect_spec
    ) ^ ExceptionDecl,

    basic_decls=List(Row(A.basic_decl, ";")[0], empty_valid=True),

    package_renaming_decl=Row(
        "package", A.static_name, A.renaming_clause, A.aspect_spec
    ) ^ PackageRenamingDecl,

    package_decl=package_decl_factory() ^ PackageDecl,
    base_package_decl=package_decl_factory() ^ BasePackageDecl,

    basic_decl=Or(
        A.body,
        A.body_stub,
        A.type_decl,
        A.task_type_decl,
        A.protected_type_decl,
        A.generic_instantiation,
        A.subprogram_decl,
        A.subtype_decl,
        A.object_decl,
        A.package_decl,
        A.aspect_clause,
        A.use_clause,
        A.exception_decl,
        A.package_renaming_decl,
        A.generic_renaming_decl,
        A.generic_decl,
        A.pragma
    ),

    object_decl=Or(
        A.sub_object_decl,
        A.single_task_decl,
        A.protected_decl,
        A.number_decl
    ),

    sub_object_decl=Row(
        A.id_list,  ":",
        Opt("aliased").as_bool(Aliased),
        Opt("constant").as_bool(Constant),
        Opt(A.mode),
        A.type_expr,
        A.default_expr,
        Opt(A.renaming_clause),
        A.aspect_spec
    ) ^ ObjectDecl,

    id_list=List(A.identifier, sep=","),

    number_decl=Row(
        A.id_list, ":", "constant", ":=",
        A.simple_expr
    ) ^ NumberDecl,

    aspect_assoc=Row(
        A.name, Opt("=>", A.expr)[1]
    ) ^ AspectAssoc,

    aspect_spec=Opt(
        Row(
            "with",
            List(A.aspect_assoc, sep=",")
        ) ^ AspectSpec
    ),

    protected_decl=Row(
        "protected", A.identifier, A.aspect_spec,
        "is", A.protected_def
    ) ^ SingleProtectedDecl,

    single_task_decl=Row("task", A.identifier, A.aspect_spec,
                         Opt(A.task_def)) ^ SingleTaskDecl,

    overriding_indicator=Or(
        Row("overriding") ^ Overriding.alt_overriding,
        Row("not", "overriding") ^ Overriding.alt_not_overriding,
        Row() ^ Overriding.alt_unspecified
    ),

    entry_decl=Row(
        A.overriding_indicator,
        "entry",
        A.identifier,
        Opt("(",
            A.constrained_subtype_indication | A.discrete_range
            | A.subtype_indication, ")")[1],
        Opt(A.param_specs),
        A.aspect_spec
    ) ^ EntryDecl,


    component_clause=Row(
        A.identifier, "at", A.simple_expr, A.range_spec
    ) ^ ComponentClause,

    aspect_clause=Or(
        Row("for", A.name, "use", A.expr) ^ AttributeDefClause,

        Row("for", A.static_name, "use", A.aggregate) ^ EnumRepClause,

        Row("for", A.static_name, "use", "record",
            Opt("at", "mod", A.simple_expr)[2],
            List(Row(A.component_clause, ";")[0], empty_valid=True),
            "end", "record") ^ RecordRepClause,

        Row("for", A.direct_name, "use", "at", A.expr) ^ AtClause
    ),

    param_spec=Row(
        List(A.identifier, sep=","),
        ":",
        Opt("aliased").as_bool(Aliased),
        Opt(A.mode),
        A.type_expr,
        Opt(":=", A.expr)[1],
    ) ^ ParamSpec,

    param_specs=Row("(", List(A.param_spec, sep=";"), ")")[1],

    subprogram_spec=Row(
        _(Or("procedure", "function")),
        Opt(A.static_name),
        Opt(
            Row(
                "(",
                List(A.param_spec, sep=";"),
                Opt(")").error()
            )[1]
        ),
        Opt("return", A.type_expr)[1]
    ) ^ SubprogramSpec,

    subprogram_decl=Or(
        subprogram_decl(_(Row("is", "null")), NullSubprogramDecl),
        subprogram_decl(_(Row("is", "abstract")), AbstractSubprogramDecl),
        subprogram_decl(
            Row("is", Or(Row("(", A.expr, ")")[1], A.aggregate))[1],
            ExprFunction
        ),
        subprogram_decl(A.renaming_clause, SubprogramRenamingDecl),
        subprogram_decl(None, SubprogramDecl)
    ),

    with_clause=Row(
        Opt("limited").as_bool(Limited),
        Opt("private").as_bool(Private),
        "with", List(A.static_name, sep=",")
    ) ^ WithClause,

    context_item=Or(A.with_clause, A.use_clause, A.pragma),

    use_clause=Or(A.use_package_clause, A.use_type_clause),

    use_package_clause=Row("use",
                           List(A.static_name, sep=",")) ^ UsePackageClause,

    use_type_clause=Row(
        "use",
        Opt("all").as_bool(All),
        "type",
        List(A.name, sep=",")
    ) ^ UseTypeClause,

    subtype_indication=Row(
        Opt("not", "null").as_bool(NotNull),
        A.subtype_name, Opt(A.constraint)
    ) ^ SubtypeIndication,

    # Rule used to disambiguate in some situations where a
    # discrete_subtype_indication is supposed to be accepted, but using the
    # general subtype_indication rule will not do:
    #
    # 1. for loop specs where you can do::
    #
    #     for A in Integer range 1 .. 12 loop
    #     end loop;
    #
    # 2. slices where you can do::
    #
    #     A (Integer range 1 .. 12)
    #
    # In those cases, using even the constrained_subtype_indication rule
    # will parse expressions as subtype indications sometimes, and even
    # cause parsing errors.

    discrete_subtype_indication=Row(
        Opt("not", "null").as_bool(NotNull),
        A.subtype_name, A.range_constraint
    ) ^ SubtypeIndication,

    constrained_subtype_indication=Row(
        Opt("not", "null").as_bool(NotNull),
        A.subtype_name, A.constraint
    ) ^ SubtypeIndication,

    type_expr=Or(
        # NOTE: Anonymous arrays are accepted where type expressions are
        # accepted. This means that you can define a function that returns an
        # anonymous array and it will be parsed correctly.
        A.anonymous_type, A.subtype_indication,
    ),

    anonymous_type=Row(A.anonymous_type_decl) ^ AnonymousType,

    mode=Or(
        Row("in", "out") ^ Mode.alt_in_out,
        Row("in") ^ Mode.alt_in,
        Row("out") ^ Mode.alt_out,
        Row() ^ Mode.alt_default
    ),

    ###########
    # Pragmas #
    ###########

    pragma_argument=Row(
        Opt(A.identifier, "=>")[0], A.expr
    ) ^ PragmaArgumentAssoc,

    pragma=Row("pragma", A.identifier,
               Opt("(", List(A.pragma_argument, ","), ")")[1]) ^ Pragma,

    subunit=Row(
        "separate", "(", A.static_name, ")",
        Or(A.subprogram_body, A.package_body, A.task_body, A.protected_body)
    ) ^ Subunit,

    library_unit_body=Or(
        A.subprogram_body, A.package_body
    ),

    library_unit_decl=Or(
        A.generic_decl,
        A.package_decl,
        A.generic_instantiation,
        A.subprogram_decl,
    ),

    library_unit_renaming_decl=Or(
        A.package_renaming_decl,
        A.generic_renaming_decl,
    ),

    library_item=Row(
        Opt("private").as_bool(Private),
        A.library_unit_body
        | A.library_unit_renaming_decl
        | A.library_unit_decl
    ) ^ LibraryItem,

    compilation_unit=Row(
        List(Row(A.context_item, ";")[0], empty_valid=True),

        Row(A.subunit | A.library_item, ";")[0],

        # Eventual pragmas attached to the body
        List(Row(A.pragma, ";")[0], empty_valid=True)
    ) ^ CompilationUnit,

    # This is the main rule. The root node will then be either:
    # * A CompilationUnit node.
    # * A list of CompilationUnit nodes.
    # * A list of pragmas.
    compilation=Or(
        # Special case for No_Body files and gnat.adc
        Row(List(Row(A.pragma, ";")[0], empty_valid=False),
            Tok(LexerToken.Termination))[0],

        # One compilation unit case
        Row(A.compilation_unit, Tok(LexerToken.Termination))[0],

        # Several compilation units case
        Row(List(A.compilation_unit, empty_valid=True),
            Tok(LexerToken.Termination))[0],
    ),

    entry_body=Row(
        "entry", A.identifier,
        Opt(Row("(", "for", A.identifier, "in",
                A.discrete_subtype_definition, ")") ^ EntryIndexSpec),
        Opt(A.param_specs),
        "when", A.expr,
        "is", A.basic_decls ^ DeclarativePart,
        Opt("begin", A.handled_stmts)[1],
        "end", _(Opt(A.static_name))
    ) ^ EntryBody,

    protected_body=Row(
        "protected", "body", A.static_name, A.aspect_spec,
        "is", A.basic_decls ^ DeclarativePart,
        "end", _(Opt(A.static_name))
    ) ^ ProtectedBody,

    protected_body_stub=Row(
        "protected", "body", A.static_name, "is", "separate",
        A.aspect_spec
    ) ^ ProtectedBodyStub,

    task_body=Row(
        "task", "body", A.static_name, A.aspect_spec,
        "is", A.basic_decls ^ DeclarativePart,
        Opt("begin", A.handled_stmts)[1],
        "end", _(Opt(A.static_name))
    ) ^ TaskBody,

    task_body_stub=Row(
        "task", "body", A.static_name,
        "is", "separate", A.aspect_spec
    ) ^ TaskBodyStub,

    package_body_stub=Row(
        "package", "body", A.static_name,
        "is", "separate", A.aspect_spec
    ) ^ PackageBodyStub,


    package_body=Row(
        "package", "body", A.static_name, A.aspect_spec,
        "is", A.basic_decls ^ DeclarativePart,
        Opt("begin", A.handled_stmts)[1],
        "end", _(Opt(A.static_name))
    ) ^ PackageBody,

    terminate_alternative=Row("terminate") ^ TerminateAlternative,

    select_stmt=Row(
        "select",
        List(
            Row(Opt("when", A.expr, "=>")[1],
                A.stmts) ^ SelectWhenPart,
            sep="or"),
        Opt("else", A.stmts)[1],
        Opt("then", "abort", A.stmts)[2],
        "end", "select"
    ) ^ SelectStmt,

    accept_stmt=Row(
        "accept", A.identifier, Opt("(", A.expr, ")")[1],
        Opt(A.param_specs),
        Opt("do", A.handled_stmts, "end", Opt(A.identifier))[1]
    ) ^ AcceptStmt,

    case_alt=Row(
        "when", A.choice_list, "=>", A.stmts
    ) ^ CaseStmtAlternative,

    case_stmt=Row(
        "case", A.expr, "is", List(A.case_alt), "end", "case"
    ) ^ CaseStmt,

    ext_return_stmt=Row(
        "return", A.sub_object_decl,
        Opt("do", A.handled_stmts, "end", "return")[1]
    ) ^ ExtendedReturnStmt,

    block_stmt=Row(
        Opt(A.identifier, ":")[0],
        Opt("declare", A.basic_decls)[1],
        "begin", A.handled_stmts, "end", _(Opt(A.identifier))
    ) ^ BlockStmt,

    loop_stmt=Row(
        Opt(A.identifier, ":")[0],
        Opt(A.iteration_scheme),
        "loop",
        A.stmts,
        "end", "loop", _(Opt(A.identifier))
    ) ^ LoopStmt,

    iteration_scheme=Or(
        Row("for", A.for_loop_param_spec)[1],
        Row("while", A.expr) ^ WhileLoopSpec
    ),

    compound_stmt=Or(A.if_stmt, A.block_stmt,
                     A.loop_stmt, A.ext_return_stmt,
                     A.case_stmt, A.accept_stmt,
                     A.select_stmt),

    if_stmt=Row(
        "if", A.expr, "then", A.stmts,
        List(Row("elsif", A.expr,
                 "then", A.stmts) ^ ElsifStmtPart,
             empty_valid=True),
        Opt("else", A.stmts)[1],
        "end", "if"
    ) ^ IfStmt,

    raise_stmt=Or(
        Row("raise", A.name, Opt("with", A.expr)[1]) ^ RaiseStmt,
        Row("raise", Null(Expr), Null(Expr)) ^ RaiseStmt,
    ),

    delay_stmt=Row(
        "delay",
        Opt("until").as_bool(Until),
        A.expr
    ) ^ DelayStmt,

    abort_stmt=Row(
        "abort", List(A.name, sep=",")
    ) ^ AbortStmt,

    body=Or(A.subprogram_body, A.package_body, A.task_body,
            A.protected_body, A.entry_body),

    body_stub=Or(A.subprogram_body_stub, A.package_body_stub,
                 A.task_body_stub, A.protected_body_stub),

    subprogram_body_stub=Row(
        A.overriding_indicator,
        A.subprogram_spec,
        "is",
        "separate",
        A.aspect_spec
    ) ^ SubprogramBodyStub,

    subprogram_body=Row(
        A.overriding_indicator,
        A.subprogram_spec,
        A.aspect_spec,
        "is",
        A.basic_decls ^ DeclarativePart,
        "begin",
        A.handled_stmts,
        "end",
        Opt(A.name)
    ) ^ SubprogramBody,

    handled_stmts=Row(
        A.stmts, Opt("exception", List(A.exception_handler))[1]
    ) ^ HandledStmts,

    exception_handler=Row(
        "when", Opt(A.identifier, ":")[0],
        List(A.name | A.others_designator, sep="|"), "=>",
        A.stmts
    ) ^ ExceptionHandler,

    stmts=List(Or(Row(A.stmt, Opt(";").error())[0],
                  A.label), empty_valid=True),

    label=Tok(Token.Label, keep=True) ^ Label,

    stmt=Or(A.compound_stmt, A.simple_stmt),

    call_stmt=Row(A.name) ^ CallStmt,

    simple_stmt=Or(A.null_stmt, A.assignment_stmt,
                   A.goto_stmt, A.exit_stmt,
                   A.return_stmt, A.requeue_stmt,
                   A.call_stmt, A.abort_stmt, A.delay_stmt,
                   A.raise_stmt, A.terminate_alternative, A.pragma),

    null_stmt=A.null_literal ^ NullStmt,

    assignment_stmt=Row(A.name, ":=", A.expr) ^ AssignStmt,

    goto_stmt=Row("goto", A.static_name) ^ GotoStmt,

    exit_stmt=Row("exit", Opt(A.identifier),
                       Opt("when", A.expr)[1]) ^ ExitStmt,

    return_stmt=(
        Row("return", Opt(A.expr | A.raise_stmt)) ^ ReturnStmt
    ),

    requeue_stmt=Row(
        "requeue", A.expr,
        Opt("with", "abort").as_bool(Abort)
    ) ^ RequeueStmt,

    identifier=Tok(Token.Identifier, keep=True) ^ Identifier,
    char_literal=Tok(Token.Char, keep=True) ^ CharLiteral,
    string_literal=Tok(Token.String, keep=True) ^ StringLiteral,

    dec_literal=Tok(Token.Decimal, keep=True) ^ RealLiteral,
    int_literal=Tok(Token.Integer, keep=True) ^ IntLiteral,
    num_literal=A.dec_literal | A.int_literal,

    null_literal=Tok(Token.Null, keep=True) ^ NullLiteral,

    allocator=Row(
        "new", Opt("(", A.name, ")")[1],
        A.qualified_name | A.subtype_indication

    ) ^ Allocator,

    for_loop_param_spec=Row(
        A.identifier,
        Opt(":", A.subtype_indication)[1],
        Or(Row("in") ^ IterType.alt_in,
           Row("of") ^ IterType.alt_of),
        Opt("reverse").as_bool(Reverse),
        A.discrete_range | A.discrete_subtype_indication | A.name
    ) ^ ForLoopSpec,

    quantified_expr=Row(
        "for", Or(Row("all") ^ Quantifier.alt_all,
                  Row("some") ^ Quantifier.alt_some),
        A.for_loop_param_spec, "=>",
        A.expr | A.discrete_range
    ) ^ QuantifiedExpr,

    case_expr=Row(
        "case", A.expr, "is",
        List(A.case_expr_alt, sep=",")
    ) ^ CaseExpr,

    case_expr_alt=Row(
        "when", A.choice_list, "=>", A.expr
    ) ^ CaseExprAlternative,


    raise_expr=Or(
        Row("raise", A.name, Opt("with", A.expr)[1]) ^ RaiseExpr,
        Row("raise", Null(Expr), Null(Expr)) ^ RaiseExpr,
    ),

    if_expr=Row(
        "if", A.expr, "then", A.expr,
        List(Row("elsif", A.expr,
                 "then", A.expr) ^ ElsifExprPart, empty_valid=True),
        Opt("else", A.expr)[1],
    ) ^ IfExpr,

    conditional_expr=Or(A.if_expr, A.case_expr,
                        A.quantified_expr),

    box_expr=Tok("<>") ^ BoxExpr,

    others_designator=Tok("others") ^ OthersDesignator,

    aggregate_field=Or(
        A.choice_list ^ AggregateMember,
        A.expr,
    ),

    aggregate_assoc=Row(
        Opt(A.aggregate_field, "=>")[0],
        Or(A.box_expr, A.expr)
    ) ^ ParamAssoc,

    aggregate_content=List(A.aggregate_assoc, sep=",") ^ ParamList,
    aggregate_content_null=Row(
        "null", "record", Null(ParamList)
    )[2],

    aggregate=Row(
        "(",
        Row(
            Opt(A.expr, "with")[0],
            Or(A.aggregate_content_null, A.aggregate_content)
        ) ^ Aggregate,
        ")")[1],

    direct_name=Or(A.identifier, A.string_literal, A.char_literal),

    param_assoc=Row(
        Opt(A.identifier | A.others_designator | A.string_literal,
            "=>")[0],
        A.expr | A.box_expr,
    ) ^ ParamAssoc,

    call_suffix=Or(
        A.discrete_subtype_indication,            # Slice via discrete subtype
        A.discrete_range,                         # Regular slice
        List(A.param_assoc, sep=",") ^ ParamList  # Regular parameter list
    ),

    # TODO: Those two rules exist only to be able to specifically parse
    # qualified expressions in the context of allocators, because using the
    # more general "name" rule will create an ambiguity::
    #
    #     new A (B);  --  Is this a call expression or a subtype indication ?
    #
    # We cannot just put the subtype_indication rule first because it will
    # generate correct parses for qualified expressions, and the underlying
    # allocator rule will fail::
    #
    #    new A'(12);
    #    --  ^ This is a valid type indication
    #
    # It would be nice to find a better way to handle this.
    #
    # - One way would be to allow left recursion across several rules, so that
    # we can have a unique rule for qualified_name.
    #
    # - One other way would be to allow "longest_parse" behavior for Or
    # parsers, so that we can use A.subtype_indication | A.name in allocator.

    qualified_name=Row(
        A.qual_name_internal, "'", Or(A.aggregate, Row("(", A.expr, ")")[1])
    ) ^ QualExpr,

    qual_name_internal=Or(
        Row(A.qual_name_internal, ".", A.direct_name) ^ DottedName,
        # Attributes
        Row(A.qual_name_internal, "'", A.identifier,
            Opt("(", A.call_suffix, ")")[1]) ^ AttributeRef,
        A.direct_name
    ),

    name=Or(
        Row(A.name, "(", A.call_suffix, ")") ^ CallExpr,
        Row(A.name, ".", A.direct_name) ^ DottedName,
        Row(A.name, ".", "all") ^ ExplicitDeref,

        # Attributes
        Row(A.name, "'", A.identifier,
            Opt("(", A.call_suffix, ")")[1]) ^ AttributeRef,

        Row(A.name, "'",
            Or(A.aggregate, Row("(", A.expr, ")")[1])) ^ QualExpr,

        A.direct_name,
    ),

    # This rule is separate from name, and doesn't accept CallExprs, because
    # since the langkit parsing engine is eager, accepting CallExprs will
    # always eat the type constraints.
    subtype_name=Or(
        Row(A.subtype_name, ".", A.direct_name) ^ DottedName,
        Row(A.subtype_name, "'", A.identifier,
            Opt("(", A.call_suffix, ")")[1]) ^ AttributeRef,
        A.direct_name,
    ),

    static_name=Or(
        Row(A.static_name, ".", A.direct_name) ^ DottedName,
        A.direct_name
    ),

    primary=Or(A.num_literal, A.null_literal,
               A.name, A.allocator,
               A.conditional_expr,
               A.raise_expr,
               A.paren_expr,
               A.aggregate),

    paren_expr=Row("(", A.expr, ")") ^ ParenExpr,

    factor=Or(
        Row(Or(Row("abs") ^ Op.alt_abs,
               Row("not") ^ Op.alt_not),
            A.primary) ^ UnOp,

        Row(A.primary, Row("**") ^ Op.alt_pow, A.primary) ^ BinOp,

        A.primary
    ),

    term=Or(
        Row(A.term, Or(Row("*") ^ Op.alt_mult,
                       Row("/") ^ Op.alt_div,
                       Row("mod") ^ Op.alt_mod,
                       Row("rem") ^ Op.alt_rem), A.factor) ^ BinOp,
        A.factor
    ),

    unop_term=Or(
        Row(Or(Row("+") ^ Op.alt_plus,
               Row("-") ^ Op.alt_minus),
            A.term) ^ UnOp,
        A.term
    ),

    simple_expr=Or(
        Row(A.simple_expr, Or(Row("+") ^ Op.alt_plus,
                              Row("-") ^ Op.alt_minus,
                              Row("&") ^ Op.alt_concat),
            A.term) ^ BinOp,
        A.unop_term
    ),

    boolean_op=Or(
        Row("xor") ^ Op.alt_xor,
        Row("and", "then") ^ Op.alt_and_then,
        Row("and") ^ Op.alt_and,
        Row("or", "else") ^ Op.alt_or_else,
        Row("or") ^ Op.alt_or,
    ),

    discrete_range=Row(A.expr,
                       Row("..") ^ Op.alt_ellipsis, A.expr) ^ BinOp,

    choice=Or(
        A.discrete_range,
        A.constrained_subtype_indication,
        A.expr,
        A.others_designator
    ),

    choice_list=List(A.choice, sep="|"),

    rel_op=Or(
        Row("not", "in") ^ Op.alt_not_in,
        Row("in") ^ Op.alt_in,
    ),

    relation=Or(
        Row(A.simple_expr,
            Or(Row("=") ^ Op.alt_eq,
               Row("/=") ^ Op.alt_neq,
               Row("<") ^ Op.alt_lt,
               Row("<=") ^ Op.alt_lte,
               Row(">") ^ Op.alt_gt,
               Row(">=") ^ Op.alt_gte),
            A.relation) ^ BinOp,

        Row(A.simple_expr, A.rel_op, A.choice_list) ^ MembershipExpr,

        A.simple_expr
    ),

    expr=Or(
        Row(A.relation, A.boolean_op, A.expr) ^ BinOp,
        A.relation
    ),

)
