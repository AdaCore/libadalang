# pyflakes off

from ada.ast import *
from ada.lexer import ada_lexer as L

# This import is after the language.ast import, because we want to be sure
# no class from langkit.expressions are shadowing the parser combinators.
from langkit.parsers import (Cut, Grammar, List, Null, Opt, Or, Pick,
                             Predicate, Skip, StopCut, _)


entry_points = {
    "compilation": "Parse an Ada source file.",
    "compilation_unit": "Parse a compilation unit.",
    "expr": "Parse an expression.",
    "pp_directive": "Parse a preprocessor directive.",
    "stmt": (
        "Parse a statement, a pragma, or a compound statement (trailing"
        " semicolon included)."
    ),
}

ada_grammar = Grammar(
    main_rule_name="compilation",
    extra_entry_points=set(entry_points) - {"compilation"},
)
A = ada_grammar
A.user_defined_rules_docs.update(entry_points)


def res(text):
    """
    Shortcut for reserved words.
    """
    return L.Identifier(match_text=text)


def sc():
    return recover(";")


def package_decl_factory(dest_class):
    """
    Factory for creating a grammar rule that parses package declarations. Used
    to be able to generate both PackageDecl and BasePackageDecl instances.

    :rtype: Parser
    """
    return dest_class(
        "package", A.defining_name, A.aspect_spec, "is",
        PublicPart(A.basic_decls.dont_skip(Or("private", "end"))),
        Opt("private", PrivatePart(A.basic_decls.dont_skip("end"))),
        end_liblevel_block(), sc()
    )


def subp_decl(dest_class, *variant_part):
    """
    Factory for subprogram declarations grammar rules.

    :param Parser variant_part: The parser for the variant part of the
        subprogram declaration.
    :param dest_class: The destination AdaNode subclass to use for the result.
    :rtype: Transform
    """
    variant_part += (A.aspect_spec, sc())
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
        "generic", keyword, A.defining_name, "renames", A.static_name,
        A.aspect_spec, ";"
    )


def recover(*rules):
    """
    Helper to parse a sequence of rules, and ignore their result, but recover
    if they're absent.
    """
    return Opt(*rules).error()


def end_liblevel_block():
    return Pick("end", Opt(EndName(A.static_name)))


def end_named_block():
    return Pick("end", Opt(EndName(A.identifier)))


A.add_rules(
    parent_list=List(A.static_name, sep="and", list_cls=ParentList),

    protected_type_decl=ProtectedTypeDecl(
        res("protected"),
        "type", A.defining_id, Opt(A.discriminant_part),
        A.aspect_spec,
        "is", Opt("new", A.parent_list, "with"),
        A.protected_def, sc()
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
        res("protected"),
        A.defining_id, A.aspect_spec,
        "is",
        Opt("new", A.parent_list, "with"),
        A.protected_def, sc()
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
        "task", "type", Cut(), A.defining_id, Opt(A.discriminant_part),
        A.aspect_spec,
        Opt(A.task_def), sc()
    ),

    subtype_decl=SubtypeDecl(
        "subtype", A.defining_id, "is", A.subtype_indication,
        A.aspect_spec, sc()
    ),

    interface_type_def=InterfaceTypeDef(
        Opt(Or(
            InterfaceKind.alt_limited("limited"),
            InterfaceKind.alt_task("task"),
            InterfaceKind.alt_protected(
                res("protected"),
            ),
            InterfaceKind.alt_synchronized(
                res("synchronized"),
            )
        )),
        res("interface"),
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
        Abstract(res("abstract")),
        Limited("limited"),
        Synchronized(
            res("synchronized")
        ),
        "new",
        A.subtype_indication,
        Opt("and", A.parent_list),
        Opt("with", A.record_def),
        WithPrivate("with", "private")
    ),

    # This rule combines, for practicality, alternatives possible only for
    # index constraints, with alternatives possible only for discriminant
    # constraints. As a results, it allows parsing stuff like `A => 1 .. 2`
    # which is not a valid constraint.
    composite_constraint_assoc=CompositeConstraintAssoc(

        # Only valid for discriminant constraints
        Opt(List(A.identifier, sep="|",
                 list_cls=DiscriminantChoiceList), "=>"),

        # Only valid for index constraints
        A.discrete_range | A.discrete_subtype_indication
        | A.expr
    ),

    composite_constraint=CompositeConstraint(
        "(", List(A.composite_constraint_assoc,
                  sep=",", list_cls=AssocList), ")"
    ),

    digits_constraint=DigitsConstraint(
        "digits", A.simple_expr, Opt(A.range_spec)
    ),

    delta_constraint=DeltaConstraint(
        "delta", A.simple_expr, Opt(A.range_spec)
    ),

    range_constraint=RangeConstraint(
        RangeSpec("range", A.discrete_range | A.name)
    ),

    constraint=Or(A.digits_constraint, A.delta_constraint,
                  A.range_constraint,
                  A.composite_constraint),

    discriminant_spec=DiscriminantSpec(
        List(A.defining_id, sep=","), ":", A.type_expr,
        Opt(":=", A.expr), Opt(A.aspect_spec)
    ),

    discr_spec_list=List(A.discriminant_spec, sep=";"),

    discriminant_part=Or(
        KnownDiscriminantPart("(", A.discr_spec_list, ")"),
        UnknownDiscriminantPart("(", "<>", ")"),
    ),

    enum_literal_decl=EnumLiteralDecl(
        A.defining_id | DefiningName(A.char_literal)
    ),

    formal_discrete_type_def=FormalDiscreteTypeDef("(", "<>", ")"),

    record_def=Or(
        RecordDef("record", A.component_list, "end", Cut(), "record"),
        NullRecordDef(
            "null", "record",
            ComponentList(
                Null(List(A.component_item)),
                Null(A.variant_part)
            ),
        ),
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
        Abstract(res("abstract")),
        Tagged(res("tagged")),
        Limited("limited"),
        A.record_def
    ),

    access_def=Or(
        AccessToSubpDef(
            NotNull("not", "null"),
            "access",
            Protected(res("protected")),
            A.subp_spec
        ),
        TypeAccessDef(
            NotNull("not", "null"),
            "access",
            All("all"),
            Constant("constant"),
            A.subtype_indication,
        )
    ),

    enum_type_def=EnumTypeDef(
        "(", List(A.enum_literal_decl, sep=","), ")",
    ),

    type_def=Or(A.record_type_def, A.real_type_def,
                A.derived_type_def, A.signed_int_type_def,
                A.mod_int_type_def, A.array_type_def, A.interface_type_def,
                A.access_def, A.formal_discrete_type_def, A.enum_type_def),

    variant=Variant(
        "when", A.choice_list, "=>", A.component_list
    ),

    anonymous_type_decl=AnonymousTypeDecl(
        Null(A.defining_id), Null(A.discriminant_part),
        Or(A.array_type_def, A.access_def)
    ),

    incomplete_type_decl=Or(
        IncompleteTaggedTypeDecl(
            "type", A.defining_id, Opt(A.discriminant_part),
            "is", Abstract(res("abstract")), res("tagged"), ";"
        ),
        IncompleteTypeDecl(
            "type", A.defining_id, Opt(A.discriminant_part), ";"
        ),
    ),

    type_decl=Or(
        ConcreteTypeDecl(
            "type", A.defining_id, Opt(A.discriminant_part),
            "is",
            Or(
                A.type_def,

                PrivateTypeDef(
                    Abstract(res("abstract")),
                    Tagged(res("tagged")),
                    Limited("limited"),
                    "private"
                ),
            ),
            A.aspect_spec, sc()
        ),
        A.incomplete_type_decl
    ),

    variant_part=VariantPart(
        "case", Cut(), A.identifier, "is",
        List(A.variant),
        "end", "case", sc()
    ),

    component_def=ComponentDef(
        Aliased(res("aliased")),
        Constant("constant"),
        A.type_expr
    ),

    component_item=Or(
        NullComponentDecl("null", sc()),
        A.component_decl,
        A.aspect_clause,
        A.pragma
    ),

    component_decl=ComponentDecl(
        List(A.defining_id, sep=","), ":", A.component_def,
        Opt(":=", A.expr), A.aspect_spec, sc()
    ),

    component_list=ComponentList(
        List(A.component_item, empty_valid=True),
        Opt(A.variant_part)
    ),

    generic_decl=Or(
        GenericSubpDecl(
            A.generic_formal_part,
            GenericSubpInternal(A.subp_spec, A.aspect_spec),
            sc()
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
        GenericFormalTypeDecl(A.formal_type_decl),
        GenericFormalSubpDecl(A.formal_subp_decl),
        GenericFormalPackage("with", A.generic_instantiation)
    ),

    formal_type_decl=Or(
        FormalTypeDecl(
            "type", A.defining_id, Opt(A.discriminant_part),
            "is",
            Or(
                A.type_def,

                PrivateTypeDef(
                    Abstract(res("abstract")),
                    Tagged(res("tagged")),
                    Limited("limited"),
                    "private"
                ),
            ),
            Opt("or", "use", A.name),
            A.aspect_spec,
            sc()
        ),
        IncompleteFormalTypeDecl(
            "type", A.defining_id, Opt(A.discriminant_part),
            Opt("is", Tagged(res(text="tagged"))),
            Opt("or", "use", A.name), ";"
        )
    ),

    formal_subp_decl=Or(
        AbstractFormalSubpDecl(
            "with", Overriding.alt_unspecified(), A.subp_spec,
            "is", res("abstract"),
            Opt(Or(A.box_expr, A.name, A.null_literal)),
            A.aspect_spec, sc()
        ),
        ConcreteFormalSubpDecl(
            "with", Overriding.alt_unspecified(), A.subp_spec,
            Opt("is", Or(A.box_expr, A.name, A.null_literal)),
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
            "package",
            A.defining_name, "is",
            "new", Cut(), A.static_name,
            Opt("(", Cut(),
                List(A.param_assoc, sep=",", list_cls=AssocList), ")"),
            A.aspect_spec, sc()
        ),

        GenericSubpInstantiation(
            A.overriding_indicator,
            Or(
                SubpKind.alt_procedure("procedure"),
                SubpKind.alt_function("function")
            ),
            A.defining_name, "is",
            "new", Cut(), A.static_name,
            Opt("(", Cut(),
                List(A.param_assoc, sep=",", list_cls=AssocList), ")"),
            A.aspect_spec, sc(),
        ),
    ),

    exception_decl=ExceptionDecl(
        A.defining_id_list, ":", "exception",
        Opt(A.renaming_clause), A.aspect_spec, sc()
    ),

    basic_decls=List(Or(A.basic_decl, Skip(ErrorDecl)), empty_valid=True),

    package_renaming_decl=PackageRenamingDecl(
        "package", A.defining_name, A.renaming_clause, A.aspect_spec, sc()
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
        A.no_type_object_renaming_decl,
        A.single_task_decl,
        A.protected_decl,
        A.number_decl
    ),

    sub_object_decl=ObjectDecl(
        A.defining_id_list,
        ":",
        Aliased(res("aliased")),
        Constant("constant"),
        Opt(A.mode),
        A.type_expr,
        Cut(),
        Opt(":=", A.expr),
        Opt(A.renaming_clause),
        A.aspect_spec,
        ";"
    ),

    no_type_object_renaming_decl=NoTypeObjectRenamingDecl(
        A.defining_id_list,
        Null(Aliased),
        Null(Constant),
        Null(Mode),
        Null(TypeExpr),
        Null(Expr),
        A.renaming_clause,
        A.aspect_spec,
        ";"
    ),

    ext_ret_stmt_object_decl=ExtendedReturnStmtObjectDecl(
        A.defining_id_list,  ":",
        Aliased(res("aliased")),
        Constant("constant"),
        Opt(A.mode),
        A.type_expr,
        Opt(":=", A.expr),
        Opt(A.renaming_clause),
        A.aspect_spec,
    ),

    defining_id_list=List(A.defining_id, sep=","),

    number_decl=NumberDecl(
        A.defining_id_list, ":", "constant",
        Cut(),
        ":=",
        A.simple_expr, sc()
    ),

    contract_case_assoc=ContractCaseAssoc(
        Or(A.expr, A.others_designator), "=>", A.expr
    ),

    contract_cases_expr=ContractCases(
        "(", List(A.contract_case_assoc, sep=","), ")"
    ),

    abstract_state_decl=Or(
        AbstractStateDecl(A.defining_name, A.aspect_spec),
        ParenAbstractStateDecl("(", A.abstract_state_decl, ")")
    ),

    multi_abstract_state_decl=Or(
        A.abstract_state_decl,
        MultiAbstractStateDecl(
            "(",
            List(
                A.abstract_state_decl,
                sep=",",
                list_cls=AbstractStateDeclList),
            ")"
        )
    ),

    aspect_assoc=Or(
        AspectAssoc(
            Identifier(L.Identifier(match_text="Abstract_State")),
            Opt("=>", Or(A.null_literal,
                         AbstractStateDeclExpr(A.multi_abstract_state_decl)))
        ),
        AspectAssoc(A.name, Opt("=>", Or(A.expr, A.contract_cases_expr)))
    ),

    aspect_spec=Opt(AspectSpec("with", Cut(), List(A.aspect_assoc, sep=","))),

    single_task_decl=SingleTaskDecl(
        "task", Cut(),
        SingleTaskTypeDecl(
            A.defining_id, Null(A.discriminant_part),
            A.aspect_spec, Opt(A.task_def)
        ), sc()
    ),

    overriding_indicator=Or(
        Overriding.alt_overriding(
            res("overriding")
        ),
        Overriding.alt_not_overriding(
            "not", res("overriding")
        ),
        Overriding.alt_unspecified()
    ),

    entry_decl=EntryDecl(
        A.overriding_indicator,
        EntrySpec(
            "entry",
            A.defining_id,
            Opt("(",
                A.constrained_subtype_indication
                | A.discrete_range
                | A.subtype_indication, ")"),
            Opt(A.param_specs),
        ),
        A.aspect_spec, sc()
    ),


    component_clause=ComponentClause(
        A.identifier, "at", A.simple_expr, A.range_spec, sc()
    ),

    aspect_clause=Or(
        EnumRepClause("for", A.static_name, "use",
                      A.regular_aggregate, Cut(), ";"),
        RecordRepClause(
            "for", A.static_name, "use", "record",
            Opt("at", "mod", A.simple_expr, sc()),
            List(Or(A.component_clause, A.pragma), empty_valid=True),
            "end", "record", sc()
        ),
        AtClause("for", A.direct_name, "use", "at", A.expr, ";"),
        # We put AttributeDefClause last, because it's the most general rule,
        # and that will allow us to get the best error recovery.
        AttributeDefClause("for", A.name, "use", Cut(), A.expr, ";"),
    ),

    param_spec=ParamSpec(
        List(A.defining_id, sep=","),
        ":",
        Aliased(res("aliased")),
        Opt(A.mode),
        A.type_expr,
        Opt(":=", A.expr),
        A.aspect_spec,
    ),

    param_specs=Params("(", Cut(), List(A.param_spec, sep=";"), ")"),

    subp_spec=SubpSpec(
        Or(SubpKind.alt_procedure("procedure"),
           SubpKind.alt_function("function")),
        Opt(A.defining_name),
        Opt(A.param_specs),
        Opt("return", A.type_expr)
    ),

    expr_fn=subp_decl(
        ExprFunction, "is", Or(A.paren_expr, A.aggregate),
    ),

    null_subp_decl=subp_decl(NullSubpDecl, "is", "null"),
    abstract_subp_decl=subp_decl(AbstractSubpDecl, "is", res("abstract")),
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
        "with", List(A.static_name, sep=","), sc()
    ),

    context_item=Or(A.with_clause, A.use_clause, A.pragma),

    use_clause=Or(A.use_package_clause, A.use_type_clause),

    use_package_clause=UsePackageClause(
        "use", List(A.static_name, sep=","), sc()
    ),

    use_type_clause=UseTypeClause("use", All("all"), "type",
                                  List(A.name, sep=","), sc()),

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
        # In our efforts to improve incomplete parsing for better parsing
        # recovery, we added `Cut` parsers in all the `DottedName` that were
        # not using it already. Unfortunatly, one of these `DottedName` didn't
        # behave correctly with a `Cut` parser because of the rules used in
        # `call_suffix`.
        #
        # Indeed, this rule parses slices and call params making the following
        # code impossible to parse with a `Cut` in the `subtype_name`` rule,
        # hence the `StopCut` below.
        #
        # Here is an example of code that do not parse without `StopCut`::
        #
        #     Call (X.all)
        #           ^^ `X.` is parsed as an incomplete subtype_name
        #             ^^^ then `all` doesn't match the expected next token
        #
        # with the corresponding parsers stack::
        #
        #     name (consumes `Call (`` in a `CallExpr`)
        #     | call_suffix
        #       | discrete_subtype_indication (consumes `X.` if no `StopCut`)
        #     then, `name` expects `)` but next token is `all`.
        NotNull("not", "null"), StopCut(A.subtype_name), A.range_constraint
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
        Opt(
            Or(
                # Enables parsing ``Post'Class => Ignore``, for example
                AttributeRef(A.identifier, "'", A.identifier,
                             Null(AssocList)),

                A.identifier
            ),
            "=>"
        ), A.expr
    ),

    pragma=Pragma(
        "pragma", Cut(), A.identifier,
        Opt("(", Cut(),
            List(Or(A.pragma_argument, A.contract_case_assoc), sep=","),
            ")"),
        sc()
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
            Skip(ErrorDecl),
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
             L.Termination),

        # One compilation unit case
        Pick(A.compilation_unit, L.Termination),

        # Several compilation units case
        Pick(List(A.compilation_unit, empty_valid=True),
             L.Termination),
    ),

    decl_part=DeclarativePart(A.basic_decls),

    entry_body=EntryBody(
        "entry", A.defining_id,
        Opt(EntryIndexSpec("(", "for", A.defining_id, "in",
                           A.discrete_subtype_definition,
                           Opt(A.aspect_spec), ")")),
        EntryCompletionFormalParams(Opt(A.param_specs)),
        A.aspect_spec,
        "when", A.expr,
        "is", Cut(),
        A.recov_decl_part,
        "begin",
        A.handled_stmts,
        end_liblevel_block(), sc()
    ),

    protected_body=ProtectedBody(
        res("protected"),
        "body", A.defining_name, A.aspect_spec,
        "is", A.decl_part.dont_skip("end"),
        end_liblevel_block(), sc()
    ),

    protected_body_stub=ProtectedBodyStub(
        res("protected"),
        "body", A.defining_name, "is", "separate",
        A.aspect_spec, sc()
    ),

    task_body=TaskBody(
        "task", "body", A.defining_name, A.aspect_spec,
        "is", Cut(), A.recov_decl_part,
        "begin",
        A.handled_stmts,
        end_liblevel_block(), sc()
    ),

    task_body_stub=TaskBodyStub(
        "task", "body", A.defining_name,
        "is", "separate", A.aspect_spec, sc()
    ),

    package_body_stub=PackageBodyStub(
        "package", "body", A.defining_name,
        "is", "separate", A.aspect_spec, sc()
    ),


    package_body=PackageBody(
        "package", "body", Cut(), A.defining_name, A.aspect_spec,
        "is", A.decl_part.dont_skip(Or("begin", "end")),
        Opt("begin", A.handled_stmts),
        end_liblevel_block(), ";"
    ),

    terminate_alternative=TerminateAlternative("terminate", sc()),

    select_stmt=SelectStmt(
        "select",
        Cut(),
        List(SelectWhenPart(
            Opt("when", A.expr, "=>"),
            A.stmts.dont_skip(Or("else", "then", "end", "or"))
        ), sep="or"),
        Opt("else", A.stmts.dont_skip(Or("end", "then"))),
        Opt("then", "abort", A.stmts.dont_skip("end")),
        "end", "select", ";"
    ),

    accept_stmt=Or(
        AcceptStmt(
            "accept", AcceptStmtBody(DefiningName(A.identifier)),
            Opt("(", A.expr, ")"),
            EntryCompletionFormalParams(Opt(A.param_specs)),
            ";"
        ),
        AcceptStmtWithStmts(
            "accept", AcceptStmtBody(DefiningName(A.identifier)),
            Opt("(", A.expr, ")"),
            EntryCompletionFormalParams(Opt(A.param_specs)),
            "do", A.handled_stmts, end_named_block(), sc()
        ),
    ),

    case_alt=CaseStmtAlternative(
        "when", Cut(), A.choice_list, "=>",
        A.stmts.dont_skip(Or("when", "end"))
    ),

    case_stmt=CaseStmt(
        "case", Cut(), A.expr, "is",
        List(A.pragma, empty_valid=True),
        List(A.case_alt),
        "end", "case", ";"
    ),

    ext_return_stmt=ExtendedReturnStmt(
        "return", A.ext_ret_stmt_object_decl, Cut(),
        Opt("do", Cut(), A.handled_stmts, "end", "return"), ";"
    ),

    iblock_stmt=Or(
        BeginBlock(
            "begin", Cut(), A.handled_stmts, end_named_block(), sc()
        ),
        DeclBlock(
            "declare", Cut(),
            A.recov_decl_part,
            "begin",
            A.handled_stmts, end_named_block(), sc()
        ),
    ),

    block_stmt=Or(
        A.iblock_stmt,
        NamedStmt(NamedStmtDecl(A.defining_id), ":", A.iblock_stmt),
    ),

    while_loop_spec=WhileLoopSpec("while", Cut(), A.expr),

    iloop_stmt=Or(
        ForLoopStmt(
            "for", Cut(),
            A.for_loop_param_spec,
            "loop",
            A.stmts.dont_skip("end"),
            "end", "loop", Opt(EndName(A.identifier)), ";"
        ),
        WhileLoopStmt(
            A.while_loop_spec,
            "loop", A.stmts.dont_skip("end"),
            "end", "loop", Opt(EndName(A.identifier)), ";"
        ),
        LoopStmt(
            Null(LoopSpec),
            "loop", Cut(), A.stmts.dont_skip("end"),
            "end", "loop", Opt(EndName(A.identifier)), ";"
        ),
    ),

    loop_stmt=Or(
        A.iloop_stmt,
        NamedStmt(NamedStmtDecl(A.defining_id), ":", A.iloop_stmt),
    ),

    compound_stmt=Or(A.if_stmt, A.block_stmt,
                     A.loop_stmt, A.ext_return_stmt,
                     A.case_stmt, A.accept_stmt,
                     A.select_stmt),

    elsif_part=ElsifStmtPart("elsif", A.expr, "then",
                             A.stmts.dont_skip(Or("elsif", "else", "end"))),

    if_stmt=IfStmt(
        "if", Cut(), A.expr, "then",
        A.stmts.dont_skip(Or("elsif", "else", "end")),
        List(A.elsif_part, empty_valid=True),
        Opt("else", A.stmts.dont_skip("end")),
        "end", "if", ";"
    ),

    raise_stmt=Or(
        RaiseStmt("raise", A.name, Opt("with", A.expr), sc()),
        RaiseStmt("raise", Null(Name), Null(Expr), sc()),
    ),

    delay_stmt=DelayStmt("delay", Until(res("until")), A.expr, sc()),

    abort_stmt=AbortStmt("abort", List(A.name, sep=","), sc()),

    body=Or(A.subp_body, A.package_body, A.task_body,
            A.protected_body, A.entry_body),

    body_stub=Or(A.subp_body_stub, A.package_body_stub,
                 A.task_body_stub, A.protected_body_stub),

    subp_body_stub=SubpBodyStub(
        A.overriding_indicator,
        A.subp_spec,
        "is",
        "separate",
        A.aspect_spec, sc()
    ),

    recov_decl_part=A.decl_part.dont_skip(Or(
        "begin", "end", "if", "for", "while", "loop",
        "declare", "accept", "select", "case"
    )),

    subp_body=SubpBody(
        A.overriding_indicator,
        A.subp_spec,
        A.aspect_spec,
        "is",
        Cut(),
        A.recov_decl_part,
        "begin",
        A.handled_stmts,
        end_liblevel_block(),
        ";"
    ),

    handled_stmts=HandledStmts(
        A.stmts.dont_skip(Or("exception", "end")),
        Opt("exception", List(A.exception_handler | A.pragma))
    ),

    exception_handler=ExceptionHandler(
        "when", Opt(A.defining_id, ":"),
        List(A.name | A.others_designator, sep="|", list_cls=AlternativesList),
        "=>", A.stmts.dont_skip(Or("when", "pragma", "end"))
    ),

    stmts=List(
        Or(A.stmt, A.label, Skip(ErrorStmt)),
        empty_valid=True, list_cls=StmtList
    ),

    label=Label("<<", Cut(), LabelDecl(A.defining_id), ">>"),

    stmt=Or(A.compound_stmt, A.simple_stmt),

    call_stmt=CallStmt(A.name, sc()),

    simple_stmt=Or(A.null_stmt, A.assignment_stmt,
                   A.goto_stmt, A.exit_stmt,
                   A.return_stmt, A.requeue_stmt,
                   A.call_stmt, A.abort_stmt, A.delay_stmt,
                   A.raise_stmt, A.terminate_alternative, A.pragma),

    null_stmt=NullStmt(L.Null, sc()),

    assignment_stmt=AssignStmt(A.name, ":=", Cut(), A.expr, sc()),

    goto_stmt=GotoStmt("goto", Cut(), A.static_name, sc()),

    exit_stmt=ExitStmt(
        "exit", Cut(), Opt(A.static_name), Opt("when", A.expr), sc()
    ),

    return_stmt=ReturnStmt("return", Opt(A.expr), sc()),

    requeue_stmt=RequeueStmt(
        res("requeue"), A.name, Abort("with", "abort"), sc()
    ),

    identifier=Identifier(L.Identifier),
    char_literal=CharLiteral(L.Char),
    string_literal=StringLiteral(L.String),

    defining_id=DefiningName(A.identifier),

    dec_literal=RealLiteral(L.Decimal),
    int_literal=IntLiteral(L.Integer),
    num_literal=A.dec_literal | A.int_literal,

    null_literal=NullLiteral(L.Null),

    allocator=Allocator(
        "new", Cut(), Opt("(", A.name, ")"),
        A.qualified_name | A.subtype_indication
    ),

    for_loop_param_spec=ForLoopSpec(
        ForLoopVarDecl(A.defining_id, Opt(":", A.type_expr)),
        IterType.alt_in("in") | IterType.alt_of("of"),
        Reverse("reverse"),
        A.discrete_range | A.discrete_subtype_indication | A.name,
        Opt("when", A.expr)
    ),

    quantified_expr=QuantifiedExpr(
        "for",
        Quantifier.alt_all("all") | Quantifier.alt_some(res("some")),
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

    box_expr=BoxExpr("<>"),

    others_designator=OthersDesignator("others"),

    iterated_assoc=IteratedAssoc(
        "for", Cut(),
        A.for_loop_param_spec, "=>",
        A.expr
    ),

    aggregate_assoc=Or(
        A.iterated_assoc,
        AggregateAssoc(
            Opt(A.choice_list, "=>"),
            Or(A.box_expr, A.expr)
        ),
    ),

    regular_aggregate=Or(
        NullRecordAggregate("(", Opt(A.expr, "with"),
                            "null", "record", Null(AssocList), ")"),
        DeltaAggregate(
            "(", A.expr, "with", "delta", Cut(),
            List(A.aggregate_assoc, sep=",", list_cls=AssocList),
            ")"
        ),

        Aggregate(
            "(", Cut(),
            Opt(A.expr, "with"),
            List(A.aggregate_assoc, sep=",", list_cls=AssocList),
            ")"
        ),
    ),

    bracket_aggregate=Or(
        BracketDeltaAggregate(
            "[", A.expr, "with", "delta", Cut(),
            List(A.aggregate_assoc, sep=",", list_cls=AssocList),
            "]"
        ),


        BracketAggregate(
            "[", Cut(),
            Opt(A.expr, "with"),
            List(A.aggregate_assoc,
                 sep=",", list_cls=AssocList, empty_valid=True),
            "]"
        ),

    ),

    aggregate=Or(
        A.regular_aggregate,
        A.bracket_aggregate
    ),

    direct_name=Or(A.identifier, A.string_literal, A.char_literal),

    param_assoc=ParamAssoc(
        Opt(A.identifier | A.others_designator | A.string_literal,
            "=>", Cut()),
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

    attr_suffix=List(A.param_assoc, sep=",", list_cls=AssocList),

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
        DottedName(A.qual_name_internal, ".", Cut(), A.direct_name),
        # Attributes, needed because of 'Class: A'Class'(...)
        AttributeRef(
            A.qual_name_internal, "'", A.identifier, Null(A.attr_suffix)
        ),
        A.direct_name
    ),

    value_sequence=ValueSequence(
        # Since parallel keyword is not supported yet, the following optional
        # parts are not parsed (RM 4.5.10):
        # - ``[parallel[(chunk_specification)] [aspect_specification]]``.
        "[", A.iterated_assoc, "]"
    ),

    name=Or(
        CallExpr(A.name, "(", Cut(), A.call_suffix, ")"),
        ExplicitDeref(A.name, ".", "all"),
        DottedName(A.name, ".", Cut(), A.direct_name),

        # Special case for 'Update
        UpdateAttributeRef(
            A.name, "'",
            Identifier(L.Identifier(match_text="Update")),
            A.update_attr_aggregate
        ),

        # Special case for 'Reduce
        ReduceAttributeRef(
            Or(A.name, A.value_sequence), "'",
            Identifier(L.Identifier(match_text="Reduce")),
            "(", A.attr_suffix, ")"
        ),

        # General Attributes
        AttributeRef(A.name, "'",
                     Predicate(A.identifier, T.Identifier.is_attr_with_args),
                     Opt("(", A.attr_suffix, ")")),

        # Class attribute
        AttributeRef(A.name, "'", A.identifier, Null(AssocList)),

        QualExpr(A.name, "'", Or(A.paren_expr, A.aggregate)),

        A.direct_name_or_target_name,
    ),
    defining_name=DefiningName(A.static_name),

    direct_name_or_target_name=Or(A.direct_name, A.target_name),

    target_name=TargetName("@"),

    update_attr_aggregate=Or(
        A.regular_aggregate,
        Aggregate("(", Cut(), Null(Expr), A.update_attr_content, ")")
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
        DottedName(A.subtype_name, ".", Cut(), A.direct_name),
        AttributeRef(A.subtype_name, "'", A.identifier,
                     Opt("(", A.attr_suffix, ")")),
        A.direct_name,
    ),

    static_name=Or(
        DottedName(A.static_name, ".", Cut(), A.direct_name),
        A.direct_name
    ),

    primary=Or(A.num_literal, A.null_literal,
               A.name, A.allocator,
               A.conditional_expr,
               A.raise_expr,
               A.paren_expr,
               A.declare_expr,
               A.aggregate),

    paren_expr=ParenExpr("(", A.expr, ")"),

    declare_expr=DeclExpr(
        "declare", List(A.object_decl, empty_valid=True), "begin", A.expr
    ),

    factor=Or(
        UnOp(Op.alt_abs("abs") | Op.alt_not("not"), Cut(), A.primary),
        BinOp(A.primary, Op.alt_pow("**"), Cut(), A.primary),
        A.primary
    ),

    term=Or(
        BinOp(A.term, Or(Op.alt_mult("*"),
                         Op.alt_div("/"),
                         Op.alt_mod("mod"),
                         Op.alt_rem("rem")), Cut(), A.factor),
        A.factor
    ),

    unop_term=Or(
        UnOp(Op.alt_plus("+") | Op.alt_minus("-"), Cut(), A.term),
        A.term
    ),

    simple_expr=Or(
        BinOp(
            A.simple_expr,
            Or(Op.alt_plus("+"), Op.alt_minus("-")),
            Cut(),
            A.term
        ),
        ConcatOp(
            A.unop_term,
            List(ConcatOperand(Op.alt_concat("&"), A.unop_term))
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

    discrete_range=Or(
        BinOp(A.simple_expr, Op.alt_double_dot(".."), A.simple_expr),
        Predicate(A.name, T.Name.is_range_attribute)
    ),

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
        DiscreteSubtypeName(A.discrete_subtype_indication),
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
            Cut(),
            A.simple_expr
        ),

        MembershipExpr(A.relation, A.rel_op, A.membership_choice_list),

        A.simple_expr
    ),

    expr=Or(
        BinOp(A.expr, A.boolean_op, Cut(), A.relation),
        A.relation
    ),

    pp_directive=Or(
        PpIfDirective("if", Cut(), A.pp_expr, A.pp_then),
        PpElsifDirective("elsif", Cut(), A.pp_expr, A.pp_then),
        PpElseDirective("else"),
        PpEndIfDirective("end", "if", ";"),
    ),

    pp_then=Opt(PpThenKw("then")),

    pp_expr=Or(
        BinOp(A.pp_expr, A.boolean_op, Cut(), A.pp_term),
        A.pp_term,
    ),

    pp_term=Or(
        ParenExpr("(", A.pp_expr, ")"),
        UnOp(Op.alt_not("not"), A.pp_expr),
        BinOp(A.identifier,
              Or(Op.alt_eq("="),
                 Op.alt_lt("<"), Op.alt_lte("<="),
                 Op.alt_gt(">"), Op.alt_gte(">=")),
              Or(A.string_literal, A.int_literal, A.identifier)),
        AttributeRef(A.identifier, "'", A.identifier, Null(AssocList)),
        A.identifier,
    ),
)
