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
        Opt("private", PrivatePart(A.basic_decls))[1],
        "end", Opt(A.static_name)
    )


def subprogram_decl(dest_class, *variant_part):
    """
    Factory for subprogram declarations grammar rules.

    :param Parser variant_part: The parser for the variant part of the
        subprogram declaration.
    :param dest_class: The destination AdaNode subclass to use for the result.
    :rtype: Transform
    """
    variant_part += (A.aspect_spec,)
    return dest_class(
        A.overriding_indicator,
        A.subprogram_spec,
        *variant_part
    )


def generic_instantiation(keyword, dest_class):
    """
    Factory for generic instantiations grammar rules.

    :param str keyword: The variant keyword that will initiate the generic
        instantiation rule.
    :param dest_class: The destination AdaNode subclass to use for the result.
    :rtype: Transform
    """
    return dest_class(
        keyword, A.static_name, "is",
        "new", A.static_name,
        Opt("(", A.call_suffix, ")")[1],
        A.aspect_spec
    )

A.add_rules(
    protected_type_decl=ProtectedTypeDecl(
        "protected", "type", A.identifier, Opt(A.discriminant_part),
        A.aspect_spec,
        "is", Opt("new", List(A.static_name, sep="and"), "with")[1],
        A.protected_def
    ),

    protected_op=Or(A.subprogram_decl, A.entry_decl, A.aspect_clause,
                    A.pragma),
    protected_el=Or(A.protected_op, A.component_decl),

    protected_def=ProtectedDef(
        PublicPart(List(Row(A.protected_op, ";")[0], empty_valid=True)),
        Opt("private",
            PrivatePart(List(Row(A.protected_el, ";")[0],
                             empty_valid=True)))[1],
        "end",
        Opt(A.identifier)
    ),

    task_item=Or(A.entry_decl, A.aspect_clause, A.pragma),

    task_def=TaskDef(
        "is",
        Opt("new", List(A.static_name, sep="and"), "with")[1],
        PublicPart(List(Row(A.task_item, ";")[0], empty_valid=True)),
        Opt("private",
            PrivatePart(List(Row(A.task_item, ";")[0], empty_valid=True)))[1],
        "end",
        Opt(A.identifier)
    ),

    task_type_decl=TaskTypeDecl(
        "task", "type", A.identifier, Opt(A.discriminant_part),
        A.aspect_spec,
        Opt(A.task_def)
    ),

    subtype_decl=SubtypeDecl(
        "subtype", A.identifier, "is", A.subtype_indication,
        A.aspect_spec
    ),

    interface_type_def=InterfaceTypeDef(
        Opt(Or(InterfaceKind.alt_limited("limited"),
               InterfaceKind.alt_task("task"),
               InterfaceKind.alt_protected("protected"),
               InterfaceKind.alt_synchronized("synchronized"))),
        "interface",
        List(Row("and", A.static_name)[1], empty_valid=True)
    ),

    array_type_def=ArrayTypeDef(
        "array",
        "(",
        Or(
            List(Row(A.static_name, "range", "<>")[0], sep=",")
            ^ UnconstrainedArrayIndices,

            List(A.discrete_subtype_definition, sep=",")
            ^ ConstrainedArrayIndices
        ),
        ")", "of", A.component_def
    ),

    discrete_subtype_definition=A.discrete_range | A.subtype_indication,

    signed_int_type_def=SignedIntTypeDef(A.range_spec),
    mod_int_type_def=ModIntTypeDef("mod", A.sexpr_or_box),

    derived_type_def=DerivedTypeDef(
        Opt("abstract").as_bool(Abstract),
        Opt("limited").as_bool(Limited),
        Opt("synchronized").as_bool(Synchronized),
        "new",
        A.subtype_indication,
        List(Row("and", A.static_name)[1], empty_valid=True),
        Opt("with", A.record_def)[1],
        Opt("with", "private").as_bool(WithPrivate)
    ),

    discriminant_assoc=DiscriminantAssoc(
        Opt(List(A.identifier, sep="|"), "=>")[0],
        A.expr
    ),

    discriminant_constraint=DiscriminantConstraint(
        "(", List(A.discriminant_assoc, sep=","), ")"
    ),

    index_constraint=IndexConstraint(
        "(", List(A.discrete_subtype_definition, sep=","), ")"
    ),

    digits_constraint=DigitsConstraint(
        "digits", A.simple_expr, Opt(A.range_spec)
    ),

    delta_constraint=DeltaConstraint(
        "delta", A.simple_expr, Opt(A.range_spec)
    ),

    range_constraint=RangeConstraint(A.range_spec),

    constraint=Or(A.digits_constraint, A.delta_constraint,
                  A.range_constraint, A.index_constraint,
                  A.discriminant_constraint),

    discriminant_spec=DiscriminantSpec(
        List(A.identifier, sep=","), ":", A.type_expr,
        A.default_expr
    ),

    discr_spec_list=List(A.discriminant_spec, sep=";"),

    discriminant_part=Or(
        KnownDiscriminantPart("(", A.discr_spec_list, ")"),
        UnknownDiscriminantPart("(", "<>", ")"),
    ),

    enum_literal_decl=(A.identifier | A.char_literal) ^ EnumLiteralDecl,

    formal_discrete_type_def=Row(
        "(", "<>", ")"
    ) ^ FormalDiscreteTypeDef,

    record_def=Or(
        RecordDef("record", A.component_list, "end", "record"),
        NullRecordDef("null", "record", Null(ComponentList)),
    ),

    range_spec=Row("range", A.discrete_range | A.name | A.box_expr)[1],

    real_type_def=Or(A.floating_point_def, A.decimal_fixed_point_def,
                     A.ordinary_fixed_point_def),

    sexpr_or_box=A.simple_expr | A.box_expr,

    ordinary_fixed_point_def=OrdinaryFixedPointDef(
        "delta", A.sexpr_or_box, Opt(A.range_spec),
    ),

    decimal_fixed_point_def=DecimalFixedPointDef(
        "delta", A.sexpr_or_box, "digits",
        A.sexpr_or_box, Opt(A.range_spec)
    ),

    floating_point_def=FloatingPointDef(
        "digits", A.sexpr_or_box, Opt(A.range_spec)
    ),

    record_type_def=RecordTypeDef(
        Opt("abstract").as_bool(Abstract),
        Opt("tagged").as_bool(Tagged),
        Opt("limited").as_bool(Limited),
        A.record_def
    ),

    access_def=Or(
        AccessToSubprogramDef(
            Opt("not", "null").as_bool(NotNull),
            "access",
            Opt("protected").as_bool(Protected),
            A.subprogram_spec
        ),
        TypeAccessDef(
            Opt("not", "null").as_bool(NotNull),
            "access",
            Opt("all").as_bool(All),
            Opt("constant").as_bool(Constant),
            A.subtype_name,
            Opt(A.constraint),
        )
    ),

    type_def=Or(A.record_type_def, A.real_type_def,
                A.derived_type_def, A.signed_int_type_def,
                A.mod_int_type_def, A.array_type_def, A.interface_type_def,
                A.access_def, A.formal_discrete_type_def),

    variant=Variant(
        "when", A.choice_list, "=>", A.component_list
    ),

    anonymous_type_decl=AnonymousTypeDecl(
        Null(A.identifier), Null(A.discriminant_part),
        Or(A.array_type_def, A.access_def),
        A.aspect_spec
    ),

    type_decl=Or(
        EnumTypeDecl(
            "type", A.identifier, "is",
            "(", List(A.enum_literal_decl, sep=","), ")",
            A.aspect_spec
        ),
        TypeDecl(
            "type", A.identifier, Opt(A.discriminant_part),
            Or(
                Row("is", A.type_def)[1],

                PrivateTypeDef(
                    "is",
                    Opt("abstract").as_bool(Abstract),
                    Opt("tagged").as_bool(Tagged),
                    Opt("limited").as_bool(Limited),
                    "private"
                ),

                IncompleteTypeDef(
                    Opt("is", "tagged").as_bool(Tagged)
                ),
            ),
            A.aspect_spec
        )
    ),

    variant_part=VariantPart(
        "case", A.identifier, "is",
        List(A.variant),
        "end", "case", ";"
    ),

    component_def=ComponentDef(
        Opt("aliased").as_bool(Aliased),
        A.type_expr
    ),

    component_item=Or(
        NullComponentDecl("null"),
        A.component_decl,
        A.aspect_clause,
        A.pragma
    ),

    default_expr=Opt(":=", A.expr)[1],

    component_decl=ComponentDecl(
        List(A.identifier, sep=","), ":", A.component_def,
        A.default_expr, A.aspect_spec
    ),

    component_list=ComponentList(
        List(Row(A.component_item, ";")[0], empty_valid=True),
        Opt(A.variant_part)
    ),

    generic_decl=Or(
        GenericSubprogramDecl(
            A.generic_formal_part, A.subprogram_spec, A.aspect_spec
        ),
        GenericPackageDecl(A.generic_formal_part, A.base_package_decl)
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

    formal_subp_decl=FormalSubpDecl(
        "with",
        A.subprogram_spec,
        _(Opt("is")),
        Opt("abstract").as_bool(Abstract),
        Opt(Or(A.box_expr, A.name, A.null_literal)),
        A.aspect_spec
    ),

    renaming_clause=RenamingClause("renames", A.name),

    generic_renaming_decl=GenericRenamingDecl(
        "generic", _(Or("package", "function", "procedure")), A.static_name,
        "renames", A.static_name, A.aspect_spec
    ),

    generic_instantiation=Or(
        generic_instantiation("package", GenericPackageInstantiation),
        generic_instantiation("procedure", GenericProcedureInstantiation),
        generic_instantiation("function", GenericFunctionInstantiation),
    ),

    exception_decl=ExceptionDecl(
        A.id_list, ":", "exception",
        Opt(A.renaming_clause), A.aspect_spec
    ),

    basic_decls=List(Row(A.basic_decl, ";")[0], empty_valid=True),

    package_renaming_decl=PackageRenamingDecl(
        "package", A.static_name, A.renaming_clause, A.aspect_spec
    ),

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

    sub_object_decl=ObjectDecl(
        A.id_list,  ":",
        Opt("aliased").as_bool(Aliased),
        Opt("constant").as_bool(Constant),
        Opt(A.mode),
        A.type_expr,
        A.default_expr,
        Opt(A.renaming_clause),
        A.aspect_spec
    ),

    id_list=List(A.identifier, sep=","),

    number_decl=NumberDecl(
        A.id_list, ":", "constant", ":=",
        A.simple_expr
    ),

    aspect_assoc=AspectAssoc(
        A.name, Opt("=>", A.expr)[1]
    ),

    aspect_spec=Opt(AspectSpec(
        "with",
        List(A.aspect_assoc, sep=",")
    )),

    protected_decl=SingleProtectedDecl(
        "protected", A.identifier, A.aspect_spec,
        "is", A.protected_def
    ),

    single_task_decl=SingleTaskDecl(
        "task", A.identifier, A.aspect_spec, Opt(A.task_def)
    ),

    overriding_indicator=Or(
        Overriding.alt_overriding("overriding"),
        Overriding.alt_not_overriding("not", "overriding"),
        Overriding.alt_unspecified()
    ),

    entry_decl=EntryDecl(
        A.overriding_indicator,
        "entry",
        A.identifier,
        Opt("(",
            A.constrained_subtype_indication | A.discrete_range
            | A.subtype_indication, ")")[1],
        Opt(A.param_specs),
        A.aspect_spec
    ),


    component_clause=ComponentClause(
        A.identifier, "at", A.simple_expr, A.range_spec
    ),

    aspect_clause=Or(
        AttributeDefClause("for", A.name, "use", A.expr),
        EnumRepClause("for", A.static_name, "use", A.aggregate),
        RecordRepClause(
            "for", A.static_name, "use", "record",
            Opt("at", "mod", A.simple_expr)[2],
            List(Row(A.component_clause, ";")[0], empty_valid=True),
            "end", "record"
        ),
        AtClause("for", A.direct_name, "use", "at", A.expr)
    ),

    param_spec=ParamSpec(
        List(A.identifier, sep=","),
        ":",
        Opt("aliased").as_bool(Aliased),
        Opt(A.mode),
        A.type_expr,
        Opt(":=", A.expr)[1],
    ),

    param_specs=Row("(", List(A.param_spec, sep=";"), ")")[1],

    subprogram_spec=SubprogramSpec(
        _(Or("procedure", "function")),
        Opt(A.static_name),
        Opt(
            Row("(",
                List(A.param_spec, sep=";"),
                Opt(")").error())[1]
        ),
        Opt("return", A.type_expr)[1]
    ),

    subprogram_decl=Or(
        subprogram_decl(NullSubprogramDecl, "is", "null"),
        subprogram_decl(AbstractSubprogramDecl, "is", "abstract"),
        subprogram_decl(
            ExprFunction,
            "is", Or(Row("(", A.expr, ")")[1], A.aggregate),
        ),
        subprogram_decl(SubprogramRenamingDecl, A.renaming_clause),
        subprogram_decl(SubprogramDecl)
    ),

    with_clause=WithClause(
        Opt("limited").as_bool(Limited),
        Opt("private").as_bool(Private),
        "with", List(A.static_name, sep=",")
    ),

    context_item=Or(A.with_clause, A.use_clause, A.pragma),

    use_clause=Or(A.use_package_clause, A.use_type_clause),

    use_package_clause=UsePackageClause("use", List(A.static_name, sep=",")),

    use_type_clause=UseTypeClause(
        "use",
        Opt("all").as_bool(All),
        "type",
        List(A.name, sep=",")
    ),

    subtype_indication=SubtypeIndication(
        Opt("not", "null").as_bool(NotNull),
        A.subtype_name, Opt(A.constraint)
    ),

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

    discrete_subtype_indication=SubtypeIndication(
        Opt("not", "null").as_bool(NotNull),
        A.subtype_name, A.range_constraint
    ),

    constrained_subtype_indication=SubtypeIndication(
        Opt("not", "null").as_bool(NotNull),
        A.subtype_name, A.constraint
    ),

    type_expr=Or(
        # NOTE: Anonymous arrays are accepted where type expressions are
        # accepted. This means that you can define a function that returns an
        # anonymous array and it will be parsed correctly.
        A.anonymous_type, A.subtype_indication,
    ),

    anonymous_type=AnonymousType(A.anonymous_type_decl),

    mode=Or(
        Mode.alt_in_out("in", "out"),
        Mode.alt_in("in"),
        Mode.alt_out("out"),
        Mode.alt_default()
    ),

    ###########
    # Pragmas #
    ###########

    pragma_argument=PragmaArgumentAssoc(
        Opt(A.identifier, "=>")[0], A.expr
    ),

    pragma=Pragma("pragma", A.identifier,
                  Opt("(", List(A.pragma_argument, ","), ")")[1]),

    subunit=Subunit(
        "separate", "(", A.static_name, ")",
        Or(A.subprogram_body, A.package_body, A.task_body, A.protected_body)
    ),

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

    library_item=LibraryItem(
        Opt("private").as_bool(Private),
        A.library_unit_body
        | A.library_unit_renaming_decl
        | A.library_unit_decl
    ),

    compilation_unit=CompilationUnit(
        List(Row(A.context_item, ";")[0], empty_valid=True),

        A.subunit | A.library_item, ";",

        # Eventual pragmas attached to the body
        List(Row(A.pragma, ";")[0], empty_valid=True)
    ),

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

    entry_body=EntryBody(
        "entry", A.identifier,
        Opt(EntryIndexSpec("(", "for", A.identifier, "in",
                           A.discrete_subtype_definition, ")")),
        Opt(A.param_specs),
        "when", A.expr,
        "is", A.basic_decls ^ DeclarativePart,
        Opt("begin", A.handled_stmts)[1],
        "end", _(Opt(A.static_name))
    ),

    protected_body=ProtectedBody(
        "protected", "body", A.static_name, A.aspect_spec,
        "is", A.basic_decls ^ DeclarativePart,
        "end", _(Opt(A.static_name))
    ),

    protected_body_stub=ProtectedBodyStub(
        "protected", "body", A.static_name, "is", "separate",
        A.aspect_spec
    ),

    task_body=TaskBody(
        "task", "body", A.static_name, A.aspect_spec,
        "is", A.basic_decls ^ DeclarativePart,
        Opt("begin", A.handled_stmts)[1],
        "end", _(Opt(A.static_name))
    ),

    task_body_stub=TaskBodyStub(
        "task", "body", A.static_name,
        "is", "separate", A.aspect_spec
    ),

    package_body_stub=PackageBodyStub(
        "package", "body", A.static_name,
        "is", "separate", A.aspect_spec
    ),


    package_body=PackageBody(
        "package", "body", A.static_name, A.aspect_spec,
        "is", A.basic_decls ^ DeclarativePart,
        Opt("begin", A.handled_stmts)[1],
        "end", _(Opt(A.static_name))
    ),

    terminate_alternative=TerminateAlternative("terminate"),

    select_stmt=SelectStmt(
        "select",
        List(SelectWhenPart(Opt("when", A.expr, "=>")[1], A.stmts), sep="or"),
        Opt("else", A.stmts)[1],
        Opt("then", "abort", A.stmts)[2],
        "end", "select"
    ),

    accept_stmt=AcceptStmt(
        "accept", A.identifier, Opt("(", A.expr, ")")[1],
        Opt(A.param_specs),
        Opt("do", A.handled_stmts, "end", Opt(A.identifier))[1]
    ),

    case_alt=CaseStmtAlternative(
        "when", A.choice_list, "=>", A.stmts
    ),

    case_stmt=CaseStmt(
        "case", A.expr, "is", List(A.case_alt), "end", "case"
    ),

    ext_return_stmt=ExtendedReturnStmt(
        "return", A.sub_object_decl,
        Opt("do", A.handled_stmts, "end", "return")[1]
    ),

    block_stmt=BlockStmt(
        Opt(A.identifier, ":")[0],
        Opt("declare", A.basic_decls)[1],
        "begin", A.handled_stmts, "end", _(Opt(A.identifier))
    ),

    loop_stmt=LoopStmt(
        Opt(A.identifier, ":")[0],
        Opt(A.iteration_scheme),
        "loop",
        A.stmts,
        "end", "loop", _(Opt(A.identifier))
    ),

    iteration_scheme=Or(
        Row("for", A.for_loop_param_spec)[1],
        WhileLoopSpec("while", A.expr)
    ),

    compound_stmt=Or(A.if_stmt, A.block_stmt,
                     A.loop_stmt, A.ext_return_stmt,
                     A.case_stmt, A.accept_stmt,
                     A.select_stmt),

    if_stmt=IfStmt(
        "if", A.expr, "then", A.stmts,
        List(Row("elsif", A.expr,
                 "then", A.stmts) ^ ElsifStmtPart,
             empty_valid=True),
        Opt("else", A.stmts)[1],
        "end", "if"
    ),

    raise_stmt=Or(
        RaiseStmt("raise", A.name, Opt("with", A.expr)[1]),
        RaiseStmt("raise", Null(Expr), Null(Expr)),
    ),

    delay_stmt=DelayStmt("delay", Opt("until").as_bool(Until), A.expr),

    abort_stmt=AbortStmt("abort", List(A.name, sep=",")),

    body=Or(A.subprogram_body, A.package_body, A.task_body,
            A.protected_body, A.entry_body),

    body_stub=Or(A.subprogram_body_stub, A.package_body_stub,
                 A.task_body_stub, A.protected_body_stub),

    subprogram_body_stub=SubprogramBodyStub(
        A.overriding_indicator,
        A.subprogram_spec,
        "is",
        "separate",
        A.aspect_spec
    ),

    subprogram_body=SubprogramBody(
        A.overriding_indicator,
        A.subprogram_spec,
        A.aspect_spec,
        "is",
        A.basic_decls ^ DeclarativePart,
        "begin",
        A.handled_stmts,
        "end",
        Opt(A.name)
    ),

    handled_stmts=HandledStmts(
        A.stmts, Opt("exception", List(A.exception_handler))[1]
    ),

    exception_handler=ExceptionHandler(
        "when", Opt(A.identifier, ":")[0],
        List(A.name | A.others_designator, sep="|"), "=>",
        A.stmts
    ),

    stmts=List(Or(Row(A.stmt, Opt(";").error())[0],
                  A.label), empty_valid=True),

    label=Tok(Token.Label, keep=True) ^ Label,

    stmt=Or(A.compound_stmt, A.simple_stmt),

    call_stmt=CallStmt(A.name),

    simple_stmt=Or(A.null_stmt, A.assignment_stmt,
                   A.goto_stmt, A.exit_stmt,
                   A.return_stmt, A.requeue_stmt,
                   A.call_stmt, A.abort_stmt, A.delay_stmt,
                   A.raise_stmt, A.terminate_alternative, A.pragma),

    null_stmt=A.null_literal ^ NullStmt,

    assignment_stmt=AssignStmt(A.name, ":=", A.expr),

    goto_stmt=GotoStmt("goto", A.static_name),

    exit_stmt=ExitStmt("exit", Opt(A.identifier),
                       Opt("when", A.expr)[1]),

    return_stmt=ReturnStmt("return", Opt(A.expr | A.raise_stmt)),

    requeue_stmt=RequeueStmt(
        "requeue", A.expr,
        Opt("with", "abort").as_bool(Abort)
    ),

    identifier=Tok(Token.Identifier, keep=True) ^ Identifier,
    char_literal=Tok(Token.Char, keep=True) ^ CharLiteral,
    string_literal=Tok(Token.String, keep=True) ^ StringLiteral,

    dec_literal=Tok(Token.Decimal, keep=True) ^ RealLiteral,
    int_literal=Tok(Token.Integer, keep=True) ^ IntLiteral,
    num_literal=A.dec_literal | A.int_literal,

    null_literal=Tok(Token.Null, keep=True) ^ NullLiteral,

    allocator=Allocator(
        "new", Opt("(", A.name, ")")[1],
        A.qualified_name | A.subtype_indication
    ),

    for_loop_param_spec=ForLoopSpec(
        A.identifier,
        Opt(":", A.subtype_indication)[1],
        IterType.alt_in("in") | IterType.alt_of("of"),
        Opt("reverse").as_bool(Reverse),
        A.discrete_range | A.discrete_subtype_indication | A.name
    ),

    quantified_expr=QuantifiedExpr(
        "for",
        Quantifier.alt_all("all") | Quantifier.alt_some("some"),
        A.for_loop_param_spec, "=>",
        A.expr | A.discrete_range
    ),

    case_expr=CaseExpr(
        "case", A.expr, "is",
        List(A.case_expr_alt, sep=",")
    ),

    case_expr_alt=CaseExprAlternative(
        "when", A.choice_list, "=>", A.expr
    ),


    raise_expr=Or(
        RaiseExpr("raise", A.name, Opt("with", A.expr)[1]),
        RaiseExpr("raise", Null(Expr), Null(Expr)),
    ),

    if_expr=IfExpr(
        "if", A.expr, "then", A.expr,
        List(ElsifExprPart("elsif", A.expr, "then", A.expr), empty_valid=True),
        Opt("else", A.expr)[1],
    ),

    conditional_expr=Or(A.if_expr, A.case_expr,
                        A.quantified_expr),

    box_expr=Tok("<>") ^ BoxExpr,

    others_designator=Tok("others") ^ OthersDesignator,

    aggregate_field=Or(
        A.choice_list ^ AggregateMember,
        A.expr,
    ),

    aggregate_assoc=ParamAssoc(
        Opt(A.aggregate_field, "=>")[0],
        Or(A.box_expr, A.expr)
    ),

    aggregate_content=List(A.aggregate_assoc, sep=",", list_cls=ParamList),
    aggregate_content_null=Row("null", "record", Null(ParamList))[2],

    aggregate=Row(
        "(",
        Aggregate(
            Opt(A.expr, "with")[0],
            Or(A.aggregate_content_null, A.aggregate_content)
        ),
        ")"
    )[1],

    direct_name=Or(A.identifier, A.string_literal, A.char_literal),

    param_assoc=ParamAssoc(
        Opt(A.identifier | A.others_designator | A.string_literal,
            "=>")[0],
        A.expr | A.box_expr,
    ),

    call_suffix=Or(
        # Slice via discrete subtype
        A.discrete_subtype_indication,

        # Regular slice
        A.discrete_range,

        # Regular parameter list
        List(A.param_assoc, sep=",", list_cls=ParamList)
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

    qualified_name=QualExpr(
        A.qual_name_internal, "'", Or(A.aggregate, Row("(", A.expr, ")")[1])
    ),

    qual_name_internal=Or(
        DottedName(A.qual_name_internal, ".", A.direct_name),
        # Attributes
        AttributeRef(
            A.qual_name_internal, "'", A.identifier,
            Opt("(", A.call_suffix, ")")[1]
        ),
        A.direct_name
    ),

    name=Or(
        CallExpr(A.name, "(", A.call_suffix, ")"),
        DottedName(A.name, ".", A.direct_name),
        ExplicitDeref(A.name, ".", "all"),

        # Attributes
        AttributeRef(A.name, "'", A.identifier,
                     Opt("(", A.call_suffix, ")")[1]),

        QualExpr(A.name, "'", Or(A.aggregate, Row("(", A.expr, ")")[1])),

        A.direct_name,
    ),

    # This rule is separate from name, and doesn't accept CallExprs, because
    # since the langkit parsing engine is eager, accepting CallExprs will
    # always eat the type constraints.
    subtype_name=Or(
        DottedName(A.subtype_name, ".", A.direct_name),
        AttributeRef(A.subtype_name, "'", A.identifier,
                     Opt("(", A.call_suffix, ")")[1]),
        A.direct_name,
    ),

    static_name=Or(
        DottedName(A.static_name, ".", A.direct_name),
        A.direct_name
    ),

    primary=Or(A.num_literal, A.null_literal,
               A.name, A.allocator,
               A.conditional_expr,
               A.raise_expr,
               A.paren_expr,
               A.aggregate),

    paren_expr=ParenExpr("(", A.expr, ")"),

    factor=Or(
        UnOp(Op.alt_abs("abs") | Op.alt_not("not"), A.primary),
        BinOp(A.primary, Op.alt_pow("**"), A.primary),
        A.primary
    ),

    term=Or(
        BinOp(A.term, Or(Op.alt_mult("*"),
                         Op.alt_div("/"),
                         Op.alt_mod("mod"),
                         Op.alt_rem("rem")), A.factor),
        A.factor
    ),

    unop_term=Or(
        UnOp(Op.alt_plus("+") | Op.alt_minus("-"), A.term),
        A.term
    ),

    simple_expr=Or(
        BinOp(
            A.simple_expr,
            Or(Op.alt_plus("+"), Op.alt_minus("-"), Op.alt_concat("&")),
            A.term
        ),
        A.unop_term
    ),

    boolean_op=Or(
        Op.alt_xor("xor"),
        Op.alt_and_then("and", "then"),
        Op.alt_and("and"),
        Op.alt_or_else("or", "else"),
        Op.alt_or("or"),
    ),

    discrete_range=BinOp(A.simple_expr, Op.alt_ellipsis(".."), A.simple_expr),

    choice=Or(
        A.discrete_range,
        A.constrained_subtype_indication,
        A.simple_expr,
        A.others_designator
    ),

    choice_list=List(A.choice, sep="|"),

    rel_op=Or(
        Op.alt_not_in("not", "in"),
        Op.alt_in("in"),
    ),

    relation=Or(
        BinOp(
            A.relation,
            Or(Op.alt_eq("="), Op.alt_neq("/="),
               Op.alt_lt("<"), Op.alt_lte("<="),
               Op.alt_gt(">"), Op.alt_gte(">=")),
            A.simple_expr
        ),

        MembershipExpr(A.relation, A.rel_op, A.choice_list),

        A.simple_expr
    ),

    expr=Or(
        BinOp(A.expr, A.boolean_op, A.relation),
        A.relation
    ),

)
