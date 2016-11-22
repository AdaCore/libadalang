from __future__ import absolute_import

from langkit.lexer import LexerToken
from language.ast import *
from language.lexer import Token

# This import is after the language.ast import, because we want to be sure
# no class from langkit.expressions are shadowing the parser combinators.
from langkit.parsers import Grammar, Row, _, Null, Tok, Opt, List, Or, Pick

ada_grammar = Grammar(main_rule_name='compilation')
A = ada_grammar


def package_decl_factory(dest_class):
    """
    Factory for creating a grammar rule that parses package declarations. Used
    to be able to generate both PackageDecl and BasePackageDecl instances.

    :rtype: Row
    """
    return dest_class(
        "package", A.static_name, A.aspect_spec, "is",
        PublicPart(A.basic_decls),
        Opt("private", PrivatePart(A.basic_decls)),
        "end", Opt(A.static_name)
    )


def subp_decl(dest_class, *variant_part):
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
        A.subp_spec,
        *variant_part
    )


def generic_renaming_decl(keyword, dest_class):
    """
    Factory for generic package/subprogram declarations grammar rules.

    :param str|Parser: Parser for the varying part of the renaming declaration.
        Typically: "package", or a parser that produces a SubpKind.
    :param ASTNode dest class: AST node that the result must produce.
    :rtype: Transform
    """
    return dest_class(
        "generic", keyword, A.static_name, "renames", A.static_name,
        A.aspect_spec
    )


def generic_instantiation(*leading_rules):
    """
    Factory for generic instantiations grammar rules.

    :param str keyword: The variant keyword that will initiate the generic
        instantiation rule.
    :param dest_class: The destination AdaNode subclass to use for the result.
    :rtype: Transform
    """
    return tuple(leading_rules) + (
        A.static_name, "is",
        "new", A.static_name,
        Opt("(", A.call_suffix, ")"),
        A.aspect_spec
    )

A.add_rules(
    protected_type_decl=ProtectedTypeDecl(
        "protected", "type", A.identifier, Opt(A.discriminant_part),
        A.aspect_spec,
        "is", Opt("new", List(A.static_name, sep="and"), "with"),
        A.protected_def
    ),

    protected_op=Or(A.subp_decl, A.entry_decl, A.aspect_clause,
                    A.pragma),
    protected_el=Or(A.protected_op, A.component_decl),

    protected_def=ProtectedDef(
        PublicPart(List(A.protected_op, ";", empty_valid=True)),
        Opt("private",
            PrivatePart(List(A.protected_el, ";", empty_valid=True))),
        "end",
        Opt(A.identifier)
    ),

    protected_decl=SingleProtectedDecl(
        "protected", A.identifier, A.aspect_spec,
        "is",
        Opt("new", List(A.static_name, sep="and"), "with"),
        A.protected_def
    ),

    task_item=Or(A.entry_decl, A.aspect_clause, A.pragma),

    task_def=TaskDef(
        "is",
        Opt("new", List(A.static_name, sep="and"), "with"),
        PublicPart(List(A.task_item, ";", empty_valid=True)),
        Opt("private", PrivatePart(List(A.task_item, ";", empty_valid=True))),
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
               InterfaceKind.alt_synchronized(Tok(Token.Identifier,
                                                  match_text="synchronized"))
               )),
        Tok(Token.Identifier, match_text="interface"),
        List("and", A.static_name, empty_valid=True)
    ),

    array_type_def=ArrayTypeDef(
        "array",
        "(",
        Or(
            UnconstrainedArrayIndices(List(A.static_name, "range", "<>",
                                           sep=",")),

            ConstrainedArrayIndices(List(A.discrete_subtype_definition,
                                         sep=","))
        ),
        ")", "of", A.component_def
    ),

    discrete_subtype_definition=A.discrete_range | A.subtype_indication,

    signed_int_type_def=SignedIntTypeDef(A.range_spec),
    mod_int_type_def=ModIntTypeDef("mod", A.sexpr_or_box),

    derived_type_def=DerivedTypeDef(
        Abstract("abstract"),
        Limited("limited"),
        Synchronized(
            Tok(Token.Identifier, match_text="synchronized")
        ),
        "new",
        A.subtype_indication,
        List("and", A.static_name, empty_valid=True),
        Opt("with", A.record_def),
        WithPrivate("with", "private")
    ),

    discriminant_assoc=DiscriminantAssoc(
        Opt(List(A.identifier, sep="|"), "=>"),
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

    enum_literal_decl=EnumLiteralDecl(A.identifier | A.char_literal),

    formal_discrete_type_def=FormalDiscreteTypeDef("(", "<>", ")"),

    record_def=Or(
        RecordDef("record", A.component_list, "end", "record"),
        NullRecordDef("null", "record", Null(ComponentList)),
    ),

    range_spec=Pick("range", A.discrete_range | A.name | A.box_expr),

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
        Abstract("abstract"),
        Tagged("tagged"),
        Limited("limited"),
        A.record_def
    ),

    access_def=Or(
        AccessToSubpDef(
            NotNull("not", "null"),
            "access",
            Protected("protected"),
            A.subp_spec
        ),
        TypeAccessDef(
            NotNull("not", "null"),
            "access",
            All("all"),
            Constant("constant"),
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
                Pick("is", A.type_def),

                PrivateTypeDef(
                    "is",
                    Abstract("abstract"),
                    Tagged("tagged"),
                    Limited("limited"),
                    "private"
                ),

                IncompleteTypeDef(Tagged("is", "tagged")),
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
        Aliased("aliased"),
        A.type_expr
    ),

    component_item=Or(
        NullComponentDecl("null"),
        A.component_decl,
        A.aspect_clause,
        A.pragma
    ),

    default_expr=Opt(":=", A.expr),

    component_decl=ComponentDecl(
        List(A.identifier, sep=","), ":", A.component_def,
        A.default_expr, A.aspect_spec
    ),

    component_list=ComponentList(
        List(A.component_item, ";", empty_valid=True),
        Opt(A.variant_part)
    ),

    generic_decl=Or(
        GenericSubpDecl(
            A.generic_formal_part, A.subp_spec, A.aspect_spec
        ),
        GenericPackageDecl(A.generic_formal_part, A.base_package_decl)
    ),

    generic_formal_part=Pick(
        "generic",
        List(A.generic_formal_decl | A.use_clause, ";", empty_valid=True)
    ),

    generic_formal_decl=Or(
        A.pragma,
        A.object_decl,
        A.type_decl,
        A.formal_subp_decl,
        Pick("with", A.generic_instantiation)
    ),

    formal_subp_decl=FormalSubpDecl(
        "with",
        Overriding.alt_unspecified(),
        A.subp_spec,

        # TODO: Refactor that kludge
        _(Opt("is")),
        Abstract("abstract"),
        Opt(Or(A.box_expr, A.name, A.null_literal)),

        A.aspect_spec
    ),

    renaming_clause=RenamingClause("renames", A.name),

    generic_renaming_decl=Or(
        generic_renaming_decl("package", GenericPackageRenamingDecl),
        generic_renaming_decl(SubpKind.alt_procedure("procedure"),
                              GenericSubpRenamingDecl),
        generic_renaming_decl(SubpKind.alt_function("function"),
                              GenericSubpRenamingDecl),
    ),

    generic_instantiation=Or(
        GenericPackageInstantiation(*generic_instantiation("package")),
        GenericSubpInstantiation(*generic_instantiation(
            A.overriding_indicator,
            Or(
                SubpKind.alt_procedure("procedure"),
                SubpKind.alt_function("function")
            )
        )),
    ),

    exception_decl=ExceptionDecl(
        A.id_list, ":", "exception",
        Opt(A.renaming_clause), A.aspect_spec
    ),

    basic_decls=List(A.basic_decl, ";", empty_valid=True),

    package_renaming_decl=PackageRenamingDecl(
        "package", A.static_name, A.renaming_clause, A.aspect_spec
    ),

    package_decl=package_decl_factory(PackageDecl),
    base_package_decl=package_decl_factory(BasePackageDecl),

    basic_decl=Or(
        A.body,
        A.body_stub,
        A.type_decl,
        A.task_type_decl,
        A.protected_type_decl,
        A.generic_instantiation,
        A.subp_decl,
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
        Aliased("aliased"),
        Constant("constant"),
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

    aspect_assoc=AspectAssoc(A.name, Opt("=>", A.expr)),

    aspect_spec=Opt(AspectSpec("with", List(A.aspect_assoc, sep=","))),

    single_task_decl=SingleTaskDecl(
        "task", A.identifier, A.aspect_spec, Opt(A.task_def)
    ),

    overriding_indicator=Or(
        Overriding.alt_overriding(
            Tok(Token.Identifier, match_text="overriding")
        ),
        Overriding.alt_not_overriding(
            "not", Tok(Token.Identifier, match_text="overriding")
        ),
        Overriding.alt_unspecified()
    ),

    entry_decl=EntryDecl(
        A.overriding_indicator,
        "entry",
        A.identifier,
        Opt("(",
            A.constrained_subtype_indication
            | A.discrete_range
            | A.subtype_indication, ")"),
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
            Opt("at", "mod", A.simple_expr),
            List(A.component_clause, ";", empty_valid=True),
            "end", "record"
        ),
        AtClause("for", A.direct_name, "use", "at", A.expr)
    ),

    param_spec=ParamSpec(
        List(A.identifier, sep=","),
        ":",
        Aliased("aliased"),
        Opt(A.mode),
        A.type_expr,
        Opt(":=", A.expr),
    ),

    param_specs=Pick("(", List(A.param_spec, sep=";"), ")"),

    subp_spec=SubpSpec(
        _(Or("procedure", "function")),
        Opt(A.static_name),
        Opt(Pick("(", List(A.param_spec, sep=";"), Opt(")").error())),
        Opt("return", A.type_expr)
    ),

    subp_decl=Or(
        subp_decl(NullSubpDecl, "is", "null"),
        subp_decl(AbstractSubpDecl, "is", "abstract"),
        subp_decl(
            ExprFunction, "is", Or(Pick("(", A.expr, ")"), A.aggregate),
        ),
        subp_decl(SubpRenamingDecl, A.renaming_clause),
        subp_decl(SubpDecl)
    ),

    with_clause=WithClause(
        Limited("limited"),
        Private("private"),
        "with", List(A.static_name, sep=",")
    ),

    context_item=Or(A.with_clause, A.use_clause, A.pragma),

    use_clause=Or(A.use_package_clause, A.use_type_clause),

    use_package_clause=UsePackageClause("use", List(A.static_name, sep=",")),

    use_type_clause=UseTypeClause("use", All("all"), "type",
                                  List(A.name, sep=",")),

    subtype_indication=SubtypeIndication(
        NotNull("not", "null"), A.subtype_name, Opt(A.constraint)
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
        NotNull("not", "null"), A.subtype_name, A.range_constraint
    ),

    constrained_subtype_indication=SubtypeIndication(
        NotNull("not", "null"), A.subtype_name, A.constraint
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
        Opt(A.identifier, "=>"), A.expr
    ),

    pragma=Pragma("pragma", A.identifier,
                  Opt("(", List(A.pragma_argument, sep=","), ")")),

    subunit=Subunit(
        "separate", "(", A.static_name, ")",
        Or(A.subp_body, A.package_body, A.task_body, A.protected_body)
    ),

    library_unit_body=Or(A.subp_body, A.package_body),

    library_unit_decl=Or(
        A.generic_decl,
        A.package_decl,
        A.generic_instantiation,
        A.subp_decl,
    ),

    library_unit_renaming_decl=Or(
        A.package_renaming_decl,
        A.generic_renaming_decl,
    ),

    library_item=LibraryItem(
        Private("private"),
        A.library_unit_body
        | A.library_unit_renaming_decl
        | A.library_unit_decl
    ),

    compilation_unit=CompilationUnit(
        List(A.context_item, ";", empty_valid=True),

        A.subunit | A.library_item, ";",

        # Eventual pragmas attached to the body
        List(A.pragma, ";", empty_valid=True)
    ),

    # This is the main rule. The root node will then be either:
    # * A CompilationUnit node.
    # * A list of CompilationUnit nodes.
    # * A list of pragmas.
    compilation=Or(
        # Special case for No_Body files and gnat.adc
        Pick(List(A.pragma, ";", empty_valid=False),
             Tok(LexerToken.Termination)),

        # One compilation unit case
        Pick(A.compilation_unit, Tok(LexerToken.Termination)),

        # Several compilation units case
        Pick(List(A.compilation_unit, empty_valid=True),
             Tok(LexerToken.Termination)),
    ),

    decl_part=DeclarativePart(A.basic_decls),

    entry_body=EntryBody(
        "entry", A.identifier,
        Opt(EntryIndexSpec("(", "for", A.identifier, "in",
                           A.discrete_subtype_definition, ")")),
        Opt(A.param_specs),
        "when", A.expr,
        "is", A.decl_part,
        Opt("begin", A.handled_stmts),
        "end", _(Opt(A.static_name))
    ),

    protected_body=ProtectedBody(
        "protected", "body", A.static_name, A.aspect_spec,
        "is", A.decl_part,
        "end", _(Opt(A.static_name))
    ),

    protected_body_stub=ProtectedBodyStub(
        "protected", "body", A.static_name, "is", "separate",
        A.aspect_spec
    ),

    task_body=TaskBody(
        "task", "body", A.static_name, A.aspect_spec,
        "is", A.decl_part,
        Opt("begin", A.handled_stmts),
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
        "is", A.decl_part,
        Opt("begin", A.handled_stmts),
        "end", _(Opt(A.static_name))
    ),

    terminate_alternative=TerminateAlternative("terminate"),

    select_stmt=SelectStmt(
        "select",
        List(SelectWhenPart(Opt("when", A.expr, "=>"), A.stmts), sep="or"),
        Opt("else", A.stmts),
        Opt("then", "abort", A.stmts),
        "end", "select"
    ),

    accept_stmt=AcceptStmt(
        "accept", A.identifier, Opt("(", A.expr, ")"),
        Opt(A.param_specs),
        Opt("do", A.handled_stmts, "end", _(Opt(A.identifier)))
    ),

    case_alt=CaseStmtAlternative(
        "when", A.choice_list, "=>", A.stmts
    ),

    case_stmt=CaseStmt(
        "case", A.expr, "is", List(A.case_alt), "end", "case"
    ),

    ext_return_stmt=ExtendedReturnStmt(
        "return", A.sub_object_decl,
        Opt("do", A.handled_stmts, "end", "return")
    ),

    block_stmt=BlockStmt(
        Opt(A.identifier, ":"),
        Opt("declare", DeclarativePart(A.basic_decls)),
        "begin", A.handled_stmts, "end", _(Opt(A.identifier))
    ),

    loop_stmt=LoopStmt(
        Opt(A.identifier, ":"),
        Opt(A.iteration_scheme),
        "loop",
        A.stmts,
        "end", "loop", _(Opt(A.identifier))
    ),

    iteration_scheme=Or(
        Pick("for", A.for_loop_param_spec),
        WhileLoopSpec("while", A.expr)
    ),

    compound_stmt=Or(A.if_stmt, A.block_stmt,
                     A.loop_stmt, A.ext_return_stmt,
                     A.case_stmt, A.accept_stmt,
                     A.select_stmt),

    if_stmt=IfStmt(
        "if", A.expr, "then", A.stmts,
        List(ElsifStmtPart("elsif", A.expr, "then", A.stmts),
             empty_valid=True),
        Opt("else", A.stmts),
        "end", "if"
    ),

    raise_stmt=Or(
        RaiseStmt("raise", A.name, Opt("with", A.expr)),
        RaiseStmt("raise", Null(Expr), Null(Expr)),
    ),

    delay_stmt=DelayStmt("delay", Until("until"), A.expr),

    abort_stmt=AbortStmt("abort", List(A.name, sep=",")),

    body=Or(A.subp_body, A.package_body, A.task_body,
            A.protected_body, A.entry_body),

    body_stub=Or(A.subp_body_stub, A.package_body_stub,
                 A.task_body_stub, A.protected_body_stub),

    subp_body_stub=SubpBodyStub(
        A.overriding_indicator,
        A.subp_spec,
        "is",
        "separate",
        A.aspect_spec
    ),

    subp_body=SubpBody(
        A.overriding_indicator,
        A.subp_spec,
        A.aspect_spec,
        "is",
        A.decl_part,
        "begin",
        A.handled_stmts,
        "end",
        Opt(A.name)
    ),

    handled_stmts=HandledStmts(
        A.stmts, Opt("exception", List(A.exception_handler))
    ),

    exception_handler=ExceptionHandler(
        "when", Opt(A.identifier, ":"),
        List(A.name | A.others_designator, sep="|"), "=>",
        A.stmts
    ),

    stmts=List(Or(Pick(A.stmt, Opt(";").error()),
                  A.label), empty_valid=True),

    label=Label(Tok(Token.Label, keep=True)),

    stmt=Or(A.compound_stmt, A.simple_stmt),

    call_stmt=CallStmt(A.name),

    simple_stmt=Or(A.null_stmt, A.assignment_stmt,
                   A.goto_stmt, A.exit_stmt,
                   A.return_stmt, A.requeue_stmt,
                   A.call_stmt, A.abort_stmt, A.delay_stmt,
                   A.raise_stmt, A.terminate_alternative, A.pragma),

    null_stmt=NullStmt(A.null_literal),

    assignment_stmt=AssignStmt(A.name, ":=", A.expr),

    goto_stmt=GotoStmt("goto", A.static_name),

    exit_stmt=ExitStmt("exit", Opt(A.identifier), Opt("when", A.expr)),

    return_stmt=ReturnStmt("return", Opt(A.expr | A.raise_stmt)),

    requeue_stmt=RequeueStmt("requeue", A.expr, Abort("with", "abort")),

    identifier=Identifier(Tok(Token.Identifier, keep=True)),
    char_literal=CharLiteral(Tok(Token.Char, keep=True)),
    string_literal=StringLiteral(Tok(Token.String, keep=True)),

    dec_literal=RealLiteral(Tok(Token.Decimal, keep=True)),
    int_literal=IntLiteral(Tok(Token.Integer, keep=True)),
    num_literal=A.dec_literal | A.int_literal,

    null_literal=NullLiteral(Tok(Token.Null, keep=True)),

    allocator=Allocator(
        "new", Opt("(", A.name, ")"),
        A.qualified_name | A.subtype_indication
    ),

    for_loop_param_spec=ForLoopSpec(
        A.identifier,
        Opt(":", A.subtype_indication),
        IterType.alt_in("in") | IterType.alt_of("of"),
        Reverse("reverse"),
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
        RaiseExpr("raise", A.name, Opt("with", A.expr)),
        RaiseExpr("raise", Null(Expr), Null(Expr)),
    ),

    if_expr=IfExpr(
        "if", A.expr, "then", A.expr,
        List(ElsifExprPart("elsif", A.expr, "then", A.expr), empty_valid=True),
        Opt("else", A.expr),
    ),

    conditional_expr=Or(A.if_expr, A.case_expr,
                        A.quantified_expr),

    box_expr=BoxExpr(Tok("<>")),

    others_designator=OthersDesignator(Tok("others")),

    aggregate_assoc=AggregateAssoc(
        Opt(A.choice_list, "=>"),
        Or(A.box_expr, A.expr)
    ),

    aggregate_content=List(A.aggregate_assoc, sep=",", list_cls=AssocList),
    aggregate_content_null=Pick("null", "record", Null(AssocList)),

    aggregate=Pick("(", Aggregate(
        Opt(A.expr, "with"), A.aggregate_content_null | A.aggregate_content
    ), ")"),

    direct_name=Or(A.identifier, A.string_literal, A.char_literal),

    param_assoc=ParamAssoc(
        Opt(A.identifier | A.others_designator | A.string_literal, "=>"),
        A.expr | A.box_expr,
    ),

    call_suffix=Or(
        # Slice via discrete subtype
        A.discrete_subtype_indication,

        # Regular slice
        A.discrete_range,

        # Regular parameter list
        List(A.param_assoc, sep=",", list_cls=AssocList)
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
        A.qual_name_internal, "'", Or(A.aggregate, Pick("(", A.expr, ")"))
    ),

    qual_name_internal=Or(
        DottedName(A.qual_name_internal, ".", A.direct_name),
        # Attributes
        AttributeRef(
            A.qual_name_internal, "'", A.identifier,
            Opt("(", A.call_suffix, ")")
        ),
        A.direct_name
    ),

    name=Or(
        CallExpr(A.name, "(", A.call_suffix, ")"),
        DottedName(A.name, ".", A.direct_name),
        ExplicitDeref(A.name, ".", "all"),

        # Attributes
        AttributeRef(A.name, "'", A.identifier, Opt("(", A.call_suffix, ")")),

        QualExpr(A.name, "'", Or(A.aggregate, Pick("(", A.expr, ")"))),

        A.direct_name,
    ),

    # This rule is separate from name, and doesn't accept CallExprs, because
    # since the langkit parsing engine is eager, accepting CallExprs will
    # always eat the type constraints.
    subtype_name=Or(
        DottedName(A.subtype_name, ".", A.direct_name),
        AttributeRef(A.subtype_name, "'", A.identifier,
                     Opt("(", A.call_suffix, ")")),
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

    discrete_range=BinOp(A.simple_expr,
                         Op.alt_double_dot(".."),
                         A.simple_expr),

    choice=Or(
        A.discrete_range,
        A.discrete_subtype_indication,
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
