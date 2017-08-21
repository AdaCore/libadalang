# pyflakes off

from __future__ import absolute_import, division, print_function

from language.ast import *
from language.lexer import ada_lexer as L

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
        end_liblevel_block(), ";"
    )


def subp_decl(dest_class, *variant_part):
    """
    Factory for subprogram declarations grammar rules.

    :param Parser variant_part: The parser for the variant part of the
        subprogram declaration.
    :param dest_class: The destination AdaNode subclass to use for the result.
    :rtype: Transform
    """
    variant_part += (A.aspect_spec, ";")
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
        A.aspect_spec, ";"
    )


def recover(*rules):
    """
    Helper to parse a sequence of rules, and ignore their result, but recover
    if they're absent.
    """
    return Opt(*rules).error()


def end_liblevel_block():
    return recover("end", Opt(A.static_name))


def end_named_block():
    return recover("end", Opt(A.identifier))


A.add_rules(
    parent_list=List(A.static_name, sep="and", list_cls=ParentList),

    protected_type_decl=ProtectedTypeDecl(
        L.Identifier(match_text="protected"),
        "type", A.identifier, Opt(A.discriminant_part),
        A.aspect_spec,
        "is", Opt("new", A.parent_list, "with"),
        A.protected_def, ";"
    ),

    protected_op=Or(A.subp_decl, A.entry_decl, A.aspect_clause, A.pragma),
    protected_el=Or(A.protected_op, A.component_decl),

    protected_def=ProtectedDef(
        PublicPart(List(A.protected_op,
                        empty_valid=True, list_cls=DeclList)),
        Opt("private",
            PrivatePart(List(A.protected_el,
                             empty_valid=True, list_cls=DeclList))),
        end_named_block()
    ),

    protected_decl=SingleProtectedDecl(
        L.Identifier(match_text="protected"),
        A.identifier, A.aspect_spec,
        "is",
        Opt("new", A.parent_list, "with"),
        A.protected_def, ";"
    ),

    task_item=Or(A.entry_decl, A.aspect_clause, A.pragma),

    task_def=TaskDef(
        "is",
        Opt("new", A.parent_list, "with"),
        PublicPart(
            List(A.task_item, empty_valid=True, list_cls=DeclList)
        ),
        Opt("private",
            PrivatePart(
                List(A.task_item, empty_valid=True, list_cls=DeclList)
            )),
        end_named_block()
    ),

    task_type_decl=TaskTypeDecl(
        "task", "type", A.identifier, Opt(A.discriminant_part),
        A.aspect_spec,
        Opt(A.task_def), ";"
    ),

    subtype_decl=SubtypeDecl(
        "subtype", A.identifier, "is", A.subtype_indication,
        A.aspect_spec, ";"
    ),

    interface_type_def=InterfaceTypeDef(
        Opt(Or(
            InterfaceKind.alt_limited("limited"),
            InterfaceKind.alt_task("task"),
            InterfaceKind.alt_protected(
                L.Identifier(match_text="protected"),
            ),
            InterfaceKind.alt_synchronized(Tok(L.Identifier,
                                           match_text="synchronized"))
        )),
        L.Identifier(match_text="interface"),
        Opt("and", A.parent_list)
    ),

    unconstrained_index=UnconstrainedArrayIndex(
        A.subtype_indication, "range", "<>"
    ),

    array_type_def=ArrayTypeDef(
        "array",
        "(",
        Or(
            UnconstrainedArrayIndices(List(A.unconstrained_index, sep=",")),
            ConstrainedArrayIndices(A.constraint_list)
        ),
        ")", "of", A.component_def
    ),

    discrete_subtype_definition=A.discrete_range | A.subtype_indication,

    constraint_list=List(A.discrete_subtype_definition, sep=",",
                         list_cls=ConstraintList),

    signed_int_type_def=SignedIntTypeDef(A.range_spec),
    mod_int_type_def=ModIntTypeDef("mod", A.sexpr_or_box),

    derived_type_def=DerivedTypeDef(
        Abstract("abstract"),
        Limited("limited"),
        Synchronized(
            L.Identifier(match_text="synchronized")
        ),
        "new",
        A.subtype_indication,
        Opt("and", A.parent_list),
        Opt("with", A.record_def),
        WithPrivate("with", "private")
    ),

    discriminant_assoc=DiscriminantAssoc(
        Opt(List(A.identifier, sep="|",
                 list_cls=DiscriminantChoiceList), "=>"),
        A.expr
    ),

    discriminant_constraint=DiscriminantConstraint(
        "(", List(A.discriminant_assoc, sep=","), ")"
    ),

    index_constraint=IndexConstraint(
        "(", A.constraint_list, ")"
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
        Opt(":=", A.expr)
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

    range_spec=RangeSpec("range", A.discrete_range | A.name | A.box_expr),

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
            Protected(
                L.Identifier(match_text="protected"),
            ),
            A.subp_spec
        ),
        TypeAccessDef(
            NotNull("not", "null"),
            "access",
            All("all"),
            Constant("constant"),
            A.subtype_indication,
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
        A.aspect_spec, PrimitivesEnvHolder()
    ),

    type_decl=Or(
        EnumTypeDecl(
            "type", A.identifier, "is",
            "(", List(A.enum_literal_decl, sep=","), ")",
            A.aspect_spec, ";"
        ),
        TypeDecl(
            "type", A.identifier, Opt(A.discriminant_part),
            "is",
            Or(
                A.type_def,

                PrivateTypeDef(
                    Abstract("abstract"),
                    Tagged("tagged"),
                    Limited("limited"),
                    "private"
                ),
            ),
            A.aspect_spec, PrimitivesEnvHolder(), ";"
        ),
        IncompleteTaggedTypeDecl(
            "type", A.identifier, Opt(A.discriminant_part),
            "is", Abstract("abstract"), "tagged", ";"
        ),
        IncompleteTypeDecl(
            "type", A.identifier, Opt(A.discriminant_part), ";"
        ),
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
        NullComponentDecl("null", ";"),
        A.component_decl,
        A.aspect_clause,
        A.pragma
    ),

    component_decl=ComponentDecl(
        List(A.identifier, sep=","), ":", A.component_def,
        Opt(":=", A.expr), A.aspect_spec, ";"
    ),

    component_list=ComponentList(
        List(A.component_item, empty_valid=True),
        Opt(A.variant_part)
    ),

    generic_decl=Or(
        GenericSubpDecl(
            A.generic_formal_part,
            GenericSubpInternal(A.subp_spec, A.aspect_spec),
            ";"
        ),
        GenericPackageDecl(A.generic_formal_part,
                           package_decl_factory(GenericPackageInternal)),
    ),

    generic_formal_part=GenericFormalPart(
        "generic",
        List(A.generic_formal_decl | A.use_clause, empty_valid=True)
    ),

    generic_formal_decl=Or(
        A.pragma,
        GenericFormalObjDecl(A.object_decl),
        GenericFormalTypeDecl(A.type_decl),
        GenericFormalSubpDecl(A.formal_subp_decl),
        GenericFormalPackage("with", A.generic_instantiation)
    ),

    formal_subp_decl=Or(
        ConcreteFormalSubpDecl(
            "with", Overriding.alt_unspecified(), A.subp_spec,
            Opt("is", Or(A.box_expr, A.name, A.null_literal)),
            A.aspect_spec, ";"
        ),
        AbstractFormalSubpDecl(
            "with", Overriding.alt_unspecified(), A.subp_spec,
            "is", "abstract",
            Opt(Or(A.box_expr, A.name, A.null_literal)),
            A.aspect_spec, ";"
        ),
    ),

    renaming_clause=RenamingClause("renames", A.name),

    generic_renaming_decl=Or(
        generic_renaming_decl("package", GenericPackageRenamingDecl),
        generic_renaming_decl(
            Or(SubpKind.alt_procedure("procedure"),
               SubpKind.alt_function("function")),
            GenericSubpRenamingDecl
        ),
    ),

    generic_instantiation=Or(
        GenericPackageInstantiation(
            EnvHolder(), "package",
            A.static_name, "is",
            "new", A.static_name,
            Opt("(", List(A.param_assoc, sep=",", list_cls=AssocList), ")"),
            A.aspect_spec, ";"
        ),

        GenericSubpInstantiation(
            EnvHolder(),
            A.overriding_indicator,
            Or(
                SubpKind.alt_procedure("procedure"),
                SubpKind.alt_function("function")
            ),
            A.static_name, "is",
            "new", A.static_name,
            Opt("(", List(A.param_assoc, sep=",", list_cls=AssocList), ")"),
            A.aspect_spec, ";",
        ),
    ),

    exception_decl=ExceptionDecl(
        A.id_list, ":", "exception",
        Opt(A.renaming_clause), A.aspect_spec, ";"
    ),

    basic_decls=List(A.basic_decl, empty_valid=True),

    package_renaming_decl=PackageRenamingDecl(
        "package", A.static_name, A.renaming_clause, A.aspect_spec, ";"
    ),

    package_decl=package_decl_factory(PackageDecl),

    basic_decl=Or(
        A.null_subp_decl,
        A.abstract_subp_decl,
        A.expr_fn,
        A.subp_renaming_decl,
        A.body_stub,
        A.generic_instantiation,
        A.body,
        A.simple_subp_decl,
        A.type_decl,
        A.task_type_decl,
        A.protected_type_decl,
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
        A.id_list,
        ":",
        Aliased("aliased"),
        Constant("constant"),
        Opt(A.mode),
        A.type_expr,
        Opt(":=", A.expr),
        Opt(A.renaming_clause),
        A.aspect_spec,
        ";"
    ),

    ext_ret_stmt_object_decl=ExtendedReturnStmtObjectDecl(
        A.id_list,  ":",
        Aliased("aliased"),
        Constant("constant"),
        Opt(A.mode),
        A.type_expr,
        Opt(":=", A.expr),
        Opt(A.renaming_clause),
        A.aspect_spec,
    ),

    id_list=List(A.identifier, sep=","),

    number_decl=NumberDecl(
        A.id_list, ":", "constant", ":=",
        A.simple_expr, ";"
    ),

    contract_case_assoc=ContractCaseAssoc(
        Or(A.expr, A.others_designator), "=>", A.expr
    ),

    contract_cases_expr=ContractCases(
        "(", List(A.contract_case_assoc, sep=","), ")"
    ),

    aspect_assoc=AspectAssoc(A.name,
                             Opt("=>", Or(A.expr, A.contract_cases_expr))),

    aspect_spec=Opt(AspectSpec("with", List(A.aspect_assoc, sep=","))),

    single_task_decl=SingleTaskDecl(
        "task",
        SingleTaskTypeDecl(
            A.identifier, Null(A.discriminant_part),
            A.aspect_spec, Opt(A.task_def)
        ), ";"
    ),

    overriding_indicator=Or(
        Overriding.alt_overriding(
            L.Identifier(match_text="overriding")
        ),
        Overriding.alt_not_overriding(
            "not", L.Identifier(match_text="overriding")
        ),
        Overriding.alt_unspecified()
    ),

    entry_decl=EntryDecl(
        A.overriding_indicator,
        "entry",
        EntrySpec(
            A.identifier,
            Opt("(",
                A.constrained_subtype_indication
                | A.discrete_range
                | A.subtype_indication, ")"),
            Opt(A.param_specs),
        ),
        A.aspect_spec, ";"
    ),


    component_clause=ComponentClause(
        A.identifier, "at", A.simple_expr, A.range_spec, ";"
    ),

    aspect_clause=Or(
        AttributeDefClause("for", A.name, "use", A.expr, ";"),
        EnumRepClause("for", A.static_name, "use", A.aggregate, ";"),
        RecordRepClause(
            "for", A.static_name, "use", "record",
            Opt("at", "mod", A.simple_expr, ";"),
            List(A.component_clause, empty_valid=True),
            recover("end", "record", ";")
        ),
        AtClause("for", A.direct_name, "use", "at", A.expr, ";")
    ),

    param_spec=ParamSpec(
        List(A.identifier, sep=","),
        ":",
        Aliased("aliased"),
        Opt(A.mode),
        A.type_expr,
        Opt(":=", A.expr),
    ),

    param_specs=Params("(", List(A.param_spec, sep=";"), ")"),

    subp_spec=SubpSpec(
        Or(SubpKind.alt_procedure("procedure"),
           SubpKind.alt_function("function")),
        Opt(A.static_name),
        Opt(A.param_specs),
        Opt("return", A.type_expr)
    ),

    expr_fn=subp_decl(
        ExprFunction, "is", Or(A.paren_expr, A.aggregate),
    ),

    null_subp_decl=subp_decl(NullSubpDecl, "is", "null"),
    abstract_subp_decl=subp_decl(AbstractSubpDecl, "is", "abstract"),
    subp_renaming_decl=subp_decl(SubpRenamingDecl, A.renaming_clause),
    simple_subp_decl=subp_decl(SubpDecl),

    subp_decl=Or(
        A.null_subp_decl,
        A.abstract_subp_decl,
        A.expr_fn,
        A.subp_renaming_decl,
        A.simple_subp_decl
    ),

    with_clause=WithClause(
        Limited("limited"),
        Private("private"),
        "with", List(A.static_name, sep=","), ";"
    ),

    context_item=Or(A.with_clause, A.use_clause, A.pragma),

    use_clause=Or(A.use_package_clause, A.use_type_clause),

    use_package_clause=UsePackageClause(
        "use", List(A.static_name, sep=","), ";"
    ),

    use_type_clause=UseTypeClause("use", All("all"), "type",
                                  List(A.name, sep=","), ";"),

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

    discrete_subtype_indication=DiscreteSubtypeIndication(
        NotNull("not", "null"), A.subtype_name, A.range_constraint
    ),

    constrained_subtype_indication=ConstrainedSubtypeIndication(
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

    pragma=Pragma(
        "pragma", A.identifier,
        Opt("(",
            List(Or(A.pragma_argument, A.contract_case_assoc), sep=","),
            ")"),
        ";"
    ),

    subunit=Subunit(
        "separate", "(", A.static_name, ")",
        Or(A.subp_body, A.package_body, A.task_body, A.protected_body)
    ),

    library_unit_body=Or(A.subp_body, A.package_body),


    library_unit_renaming_decl=Or(
        A.package_renaming_decl,
        A.generic_renaming_decl,
    ),

    library_item=LibraryItem(
        Private("private"),
        Or(
            A.null_subp_decl,
            A.abstract_subp_decl,
            A.expr_fn,
            A.subp_renaming_decl,
            A.generic_instantiation,
            A.library_unit_body,
            A.simple_subp_decl,
            A.library_unit_renaming_decl,
            A.generic_decl,
            A.package_decl,
        )
    ),

    compilation_unit=CompilationUnit(
        List(A.context_item, empty_valid=True),

        A.subunit | A.library_item,

        # Eventual pragmas attached to the body
        List(A.pragma, empty_valid=True)
    ),

    # This is the main rule. The root node will then be either:
    # * A CompilationUnit node.
    # * A list of CompilationUnit nodes.
    # * A list of pragmas.
    compilation=Or(
        # Special case for No_Body files and gnat.adc
        Pick(List(A.pragma, empty_valid=False),
             L.Termination()),

        # One compilation unit case
        Pick(A.compilation_unit, L.Termination()),

        # Several compilation units case
        Pick(List(A.compilation_unit, empty_valid=True),
             L.Termination()),
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
        end_liblevel_block(), ";"
    ),

    protected_body=ProtectedBody(
        L.Identifier(match_text="protected"),
        "body", A.static_name, A.aspect_spec,
        "is", A.decl_part,
        end_liblevel_block(), ";"
    ),

    protected_body_stub=ProtectedBodyStub(
        L.Identifier(match_text="protected"),
        "body", A.static_name, "is", "separate",
        A.aspect_spec, ";"
    ),

    task_body=TaskBody(
        "task", "body", A.static_name, A.aspect_spec,
        "is", A.decl_part,
        Opt("begin", A.handled_stmts),
        end_liblevel_block(), ";"
    ),

    task_body_stub=TaskBodyStub(
        "task", "body", A.static_name,
        "is", "separate", A.aspect_spec, ";"
    ),

    package_body_stub=PackageBodyStub(
        "package", "body", A.static_name,
        "is", "separate", A.aspect_spec, ";"
    ),


    package_body=PackageBody(
        "package", "body", A.static_name, A.aspect_spec,
        "is", A.decl_part,
        Opt("begin", A.handled_stmts),
        end_liblevel_block(), ";"
    ),

    terminate_alternative=TerminateAlternative("terminate", ";"),

    select_stmt=SelectStmt(
        "select",
        List(SelectWhenPart(Opt("when", A.expr, "=>"), A.stmts), sep="or"),
        Opt("else", A.stmts),
        Opt("then", "abort", A.stmts),
        recover("end", "select"), ";"
    ),

    accept_stmt=Or(
        AcceptStmt(
            "accept", A.identifier, Opt("(", A.expr, ")"),
            Opt(A.param_specs), ";"
        ),
        AcceptStmtWithStmts(
            "accept", A.identifier, Opt("(", A.expr, ")"),
            Opt(A.param_specs),
            "do", A.handled_stmts, end_named_block(), ";"
        ),
    ),

    case_alt=CaseStmtAlternative(
        "when", A.choice_list, "=>", A.stmts
    ),

    case_stmt=CaseStmt(
        "case", A.expr, "is", List(A.case_alt),
        recover("end", "case"), ";"
    ),

    ext_return_stmt=ExtendedReturnStmt(
        "return", A.ext_ret_stmt_object_decl,
        Opt("do", A.handled_stmts, recover("end", "return")), ";"
    ),

    iblock_stmt=Or(
        BeginBlock(
            "begin", A.handled_stmts, end_named_block(), ";"
        ),
        DeclBlock(
            "declare", DeclarativePart(A.basic_decls),
            recover("begin"), A.handled_stmts, end_named_block(), ";"
        ),
    ),

    block_stmt=Or(
        A.iblock_stmt,
        NamedStmt(NamedStmtDecl(A.identifier), ":", A.iblock_stmt),
    ),

    iloop_stmt=Or(
        ForLoopStmt(
            "for", A.for_loop_param_spec,
            "loop", A.stmts, recover("end", "loop"), Opt(A.identifier), ";"
        ),
        WhileLoopStmt(
            WhileLoopSpec("while", A.expr),
            "loop", A.stmts, recover("end", "loop"), Opt(A.identifier), ";"
        ),
        LoopStmt(
            Null(LoopSpec),
            "loop", A.stmts, recover("end", "loop"), Opt(A.identifier), ";"
        ),
    ),

    loop_stmt=Or(
        A.iloop_stmt,
        NamedStmt(NamedStmtDecl(A.identifier), ":", A.iloop_stmt),
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
        recover("end", "if"), ";"
    ),

    raise_stmt=Or(
        RaiseStmt("raise", A.name, Opt("with", A.expr), ";"),
        RaiseStmt("raise", Null(Name), Null(Expr), ";"),
    ),

    delay_stmt=DelayStmt("delay", Until("until"), A.expr, ";"),

    abort_stmt=AbortStmt("abort", List(A.name, sep=","), ";"),

    body=Or(A.subp_body, A.package_body, A.task_body,
            A.protected_body, A.entry_body),

    body_stub=Or(A.subp_body_stub, A.package_body_stub,
                 A.task_body_stub, A.protected_body_stub),

    subp_body_stub=SubpBodyStub(
        A.overriding_indicator,
        A.subp_spec,
        "is",
        "separate",
        A.aspect_spec, ";"
    ),

    subp_body=SubpBody(
        A.overriding_indicator,
        A.subp_spec,
        A.aspect_spec,
        "is",
        A.decl_part,
        recover("begin"),
        A.handled_stmts,
        end_liblevel_block(),
        ";"
    ),

    handled_stmts=HandledStmts(
        A.stmts,
        Opt("exception", List(A.exception_handler | A.pragma))
    ),

    exception_handler=ExceptionHandler(
        "when", Opt(A.identifier, ":"),
        List(A.name | A.others_designator, sep="|", list_cls=AlternativesList),
        "=>", A.stmts
    ),

    stmts=List(
        Or(A.stmt, A.label),
        empty_valid=True, list_cls=StmtList
    ),

    label=Label("<<", LabelDecl(A.identifier), ">>"),

    stmt=Or(A.compound_stmt, A.simple_stmt),

    call_stmt=CallStmt(A.name, ";"),

    simple_stmt=Or(A.null_stmt, A.assignment_stmt,
                   A.goto_stmt, A.exit_stmt,
                   A.return_stmt, A.requeue_stmt,
                   A.call_stmt, A.abort_stmt, A.delay_stmt,
                   A.raise_stmt, A.terminate_alternative, A.pragma),

    null_stmt=NullStmt(A.null_literal, ";"),

    assignment_stmt=AssignStmt(A.name, ":=", A.expr, ";"),

    goto_stmt=GotoStmt("goto", A.static_name, ";"),

    exit_stmt=ExitStmt("exit", Opt(A.identifier), Opt("when", A.expr), ";"),

    return_stmt=ReturnStmt("return", Opt(A.expr), ";"),

    requeue_stmt=RequeueStmt("requeue", A.expr, Abort("with", "abort"), ";"),

    identifier=Identifier(L.Identifier(keep=True)),
    char_literal=CharLiteral(L.Char(keep=True)),
    string_literal=StringLiteral(L.String(keep=True)),

    dec_literal=RealLiteral(L.Decimal(keep=True)),
    int_literal=IntLiteral(L.Integer(keep=True)),
    num_literal=A.dec_literal | A.int_literal,

    null_literal=NullLiteral(L.Null(keep=True)),

    allocator=Allocator(
        "new", Opt("(", A.name, ")"),
        A.qualified_name | A.subtype_indication
    ),

    for_loop_param_spec=ForLoopSpec(
        ForLoopVarDecl(A.identifier, Opt(":", A.subtype_indication)),
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
        RaiseExpr("raise", Null(Name), Null(Expr)),
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

    aggregate=Or(
        Aggregate(
            "(", Opt(A.expr, "with"),
            List(A.aggregate_assoc, sep=",", list_cls=AssocList),
            ")"
        ),
        NullRecordAggregate("(", Opt(A.expr, "with"),
                            "null", "record", Null(AssocList), ")")
    ),

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
        A.qual_name_internal, "'", Or(A.paren_expr, A.aggregate)
    ),

    qual_name_internal=Or(
        DottedName(A.qual_name_internal, ".", A.direct_name),
        # Attributes, needed because of 'Class: A'Class'(...)
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

        # Special case for 'Update
        UpdateAttributeRef(
            A.name, "'",
            Identifier(L.Identifier(match_text="Update", keep=True)),
            A.update_attr_aggregate
        ),

        # General Attributes
        AttributeRef(A.name, "'", A.identifier, Opt("(", A.call_suffix, ")")),

        QualExpr(A.name, "'", Or(A.paren_expr, A.aggregate)),

        A.direct_name,
    ),

    update_attr_aggregate=Or(
        A.aggregate,
        Aggregate("(", Null(Expr), A.update_attr_content, ")")
    ),

    update_attr_content=List(A.multidim_array_assoc, sep=",",
                             list_cls=AssocList),

    multidim_array_assoc=MultiDimArrayAssoc(
        List(A.aggregate, sep="|", list_cls=AlternativesList), "=>", A.expr,
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
        A.expr,
        A.others_designator
    ),

    choice_list=List(A.choice, sep="|", list_cls=AlternativesList),

    rel_op=Or(
        Op.alt_not_in("not", "in"),
        Op.alt_in("in"),
    ),

    membership_choice=Or(
        A.discrete_range,
        DiscreteSubtypeExpr(A.discrete_subtype_indication),
        A.simple_expr,
    ),

    membership_choice_list=List(
        A.membership_choice, sep="|", list_cls=ExprAlternativesList
    ),

    relation=Or(
        RelationOp(
            A.relation,
            Or(Op.alt_eq("="), Op.alt_neq("/="),
               Op.alt_lt("<"), Op.alt_lte("<="),
               Op.alt_gt(">"), Op.alt_gte(">=")),
            A.simple_expr
        ),

        MembershipExpr(A.relation, A.rel_op, A.membership_choice_list),

        A.simple_expr
    ),

    expr=Or(
        BinOp(A.expr, A.boolean_op, A.relation),
        A.relation
    ),

)
