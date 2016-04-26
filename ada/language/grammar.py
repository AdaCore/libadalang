from __future__ import absolute_import

from language.ast import *
from language.lexer import Token

# This import is after the language.ast import, because we want to be sure
# no class from langkit.expressions are shadowing the parser combinators.
from langkit.parsers import Grammar, Row, Enum, _, Null, Tok, Opt, List, Or

ada_grammar = Grammar()
A = ada_grammar
ada_grammar.main_rule_name = "compilation_unit"


def package_decl_factory():
    """
    Factory for creating a grammar rule that parses package declarations. Used
    to be able to generate both PackageDecl and BasePackageDecl instances.

    :rtype: Row
    """
    return Row(
        "package", A.static_name, A.aspect_specification, "is",
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
        A.aspect_specification,
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
        A.aspect_specification
    ) ^ dest_class

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
        "task", "type", A.identifier, Opt(A.type_discriminant),
        A.aspect_specification,
        Opt(A.task_def)
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
            List(Row(A.static_name, "range", "<>")[0], sep=",")
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
        A.access_expression,
        Opt(A.constraint),
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

    generic_decl=Or(
        Row(A.generic_formal_part, A.subprogram_spec,
            A.aspect_specification) ^ GenericSubprogramDecl,
        Row(A.generic_formal_part, A.base_package_decl) ^ GenericPackageDecl
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
        Opt(Or(A.diamond_expr, A.name, A.null_literal)),
        A.aspect_specification
    ) ^ FormalSubpDecl,

    renaming_clause=Row("renames", A.name) ^ RenamingClause,

    generic_renaming_decl=Row(
        "generic", _(Or("package", "function", "procedure")), A.static_name,
        "renames", A.static_name, A.aspect_specification
    ) ^ GenericRenamingDecl,

    generic_instantiation=Or(
        generic_instantiation("package", GenericPackageInstantiation),
        generic_instantiation("procedure", GenericProcedureInstantiation),
        generic_instantiation("function", GenericFunctionInstantiation),
    ),

    exception_decl=Row(
        A.id_list, ":", "exception",
        Opt(A.renaming_clause), A.aspect_specification
    ) ^ ExceptionDecl,

    basic_decls=List(Row(A.basic_decl, ";")[0], empty_valid=True),

    package_renaming_decl=Row(
        "package", A.static_name, A.renaming_clause, A.aspect_specification
    ) ^ PackageRenamingDecl,

    package_decl=package_decl_factory() ^ PackageDecl,
    base_package_decl=package_decl_factory() ^ BasePackageDecl,

    basic_decl=Or(
        A.body,
        A.body_stub,
        A.full_type_decl,
        A.task_type_decl,
        A.protected_type_decl,
        A.generic_instantiation,
        A.subprogram_decl,
        A.subtype_decl,
        A.object_decl,
        A.package_decl,
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
                  Opt(A.task_def)) ^ TaskDecl,

    overriding_indicator=Or(
        Enum("overriding", Overriding("overriding")),
        Enum(Row("not", "overriding"), Overriding("not_overriding")),
        Enum(None, Overriding("unspecified"))
    ),

    entry_decl=Row(
        A.overriding_indicator,
        "entry",
        A.identifier,
        Opt("(",
            A.constrained_type_ref | A.discrete_range | A.type_ref, ")")[1],
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
        Opt(A.static_name),
        Opt(
            Row(
                "(",
                List(A.parameter_profile, sep=";"),
                Opt(")").error()
            )[1]
        ),
        Opt("return", A.type_expression)[1]
    ) ^ SubprogramSpec,

    subprogram_decl=Or(
        subprogram_decl(_(Row("is", "null")), NullSubprogramDecl),
        subprogram_decl(_(Row("is", "abstract")), AbstractSubprogramDecl),
        subprogram_decl(
            Row("is", Or(Row("(", A.expression, ")")[1], A.aggregate))[1],
            ExpressionFunction
        ),
        subprogram_decl(A.renaming_clause, RenamingSubprogramDecl),
        subprogram_decl(None, SubprogramDecl)
    ),

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
        Opt("private").as_bool(),
        Or(A.library_unit_body,
           A.library_unit_decl,
           A.library_unit_renaming_decl)
    ) ^ LibraryItem,

    compilation_unit=Row(
        List(Row(A.context_item, ";")[0], empty_valid=True),
        List(Row(A.library_item | A.subunit |
                 A.generic_instantiation | A.pragma, ";")[0],
             empty_valid=True),
    ) ^ CompilationUnit,

    entry_body=Row(
        "entry", A.identifier,
        Opt(Row("(", "for", A.identifier, "in",
                A.discrete_subtype_definition, ")") ^ EntryIndexSpec),
        Opt(A.parameter_profiles),
        "when", A.expression,
        "is", A.basic_decls,
        Opt("begin", A.handled_statements)[1],
        "end", _(Opt(A.static_name))
    ) ^ EntryBody,

    protected_body=Row(
        "protected", "body", A.static_name, A.aspect_specification,
        "is", A.basic_decls,
        "end", _(Opt(A.static_name))
    ) ^ ProtectedBody,

    protected_body_stub=Row(
        "protected", "body", A.static_name, "is", "separate",
        A.aspect_specification
    ) ^ ProtectedBodyStub,

    task_body=Row(
        "task", "body", A.static_name, A.aspect_specification,
        "is", A.basic_decls,
        Opt("begin", A.handled_statements)[1],
        "end", _(Opt(A.static_name))
    ) ^ TaskBody,

    task_body_stub=Row(
        "task", "body", A.static_name,
        "is", "separate", A.aspect_specification
    ) ^ TaskBodyStub,

    package_body_stub=Row(
        "package", "body", A.static_name,
        "is", "separate", A.aspect_specification
    ) ^ PackageBodyStub,


    package_body=Row(
        "package", "body", A.static_name, A.aspect_specification,
        "is", A.basic_decls,
        Opt("begin", A.handled_statements)[1],
        "end", _(Opt(A.static_name))
    ) ^ PackageBody,

    terminate_statement=Row("terminate") ^ TerminateStatement,

    select_statement=Row(
        "select",
        List(
            Row(Opt("when", A.expression, "=>")[1],
                A.statements) ^ SelectWhenPart,
            sep="or"),
        Opt("else", A.statements)[1],
        Opt("then", "abort", A.statements)[2],
        "end", "select"
    ) ^ SelectStatement,

    accept_statement=Row(
        "accept", A.identifier, Opt("(", A.expression, ")")[1],
        Opt(A.parameter_profiles),
        Opt("do", A.handled_statements, "end", Opt(A.identifier))[1]
    ) ^ AcceptStatement,

    case_alt=Row(
        "when", A.choice_list, "=>", A.statements
    ) ^ CaseStatementAlternative,

    case_statement=Row(
        "case", A.expression, "is", List(A.case_alt), "end", "case"
    ) ^ CaseStatement,

    ext_return_statement=Row(
        "return", A.sub_object_decl,
        Opt("do", A.handled_statements, "end", "return")[1]
    ) ^ ExtReturnStatement,

    block_statement=Row(
        Opt(A.identifier, ":")[0],
        Opt("declare", A.basic_decls)[1],
        "begin", A.handled_statements, "end", _(Opt(A.identifier))
    ) ^ BlockStatement,

    loop_statement=Row(
        Opt(A.identifier, ":")[0],
        Opt(A.iteration_scheme),
        "loop",
        A.statements,
        "end", "loop", _(Opt(A.identifier))
    ) ^ LoopStatement,

    iteration_scheme=Or(
        Row("for", A.for_loop_parameter_spec)[1],
        Row("while", A.expression) ^ WhileLoopSpec
    ),

    compound_statement=Or(A.if_statement, A.block_statement,
                          A.loop_statement, A.ext_return_statement,
                          A.case_statement, A.accept_statement,
                          A.select_statement),

    if_statement=Row(
        "if", A.expression, "then", A.statements,
        List(Row("elsif", A.expression,
                 "then", A.statements) ^ ElsifStatementPart,
             empty_valid=True),
        Opt("else", A.statements)[1],
        "end", "if"
    ) ^ IfStatement,

    raise_statement=Or(
        Row("raise", A.name, Opt("with", A.expression)[1]) ^ RaiseStatement,
        Row("raise", Null(Expr), Null(Expr)) ^ RaiseStatement,
    ),

    delay_statement=Row(
        "delay", Opt("until").as_bool(), A.expression
    ) ^ DelayStatement,

    abort_statement=Row(
        "abort", List(A.name, sep=",")
    ) ^ AbortStatement,

    body=Or(A.subprogram_body, A.package_body, A.task_body,
            A.protected_body, A.entry_body),

    body_stub=Or(A.subprogram_body_stub, A.package_body_stub,
                 A.task_body_stub, A.protected_body_stub),

    subprogram_body_stub=Row(
        A.overriding_indicator,
        A.subprogram_spec,
        "is",
        "separate",
        A.aspect_specification
    ) ^ SubprogramBodyStub,

    subprogram_body=Row(
        A.overriding_indicator,
        A.subprogram_spec,
        A.aspect_specification,
        "is",
        A.basic_decls,
        "begin",
        A.handled_statements,
        "end",
        Opt(A.name)
    ) ^ SubprogramBody,

    handled_statements=Row(
        A.statements, Opt("exception", List(A.exception_handler))[1]
    ) ^ HandledStatements,

    exception_handler=Row(
        "when", Opt(A.identifier, ":")[0],
        List(A.name | A.others_designator, sep="|"), "=>",
        A.statements
    ) ^ ExceptionHandler,

    statements=List(Or(Row(A.statement, Opt(";").error())[0],
                       A.label), empty_valid=True),

    label=Tok(Token.Label) ^ Label,

    statement=Or(A.compound_statement, A.simple_statement),

    call_statement=Row(A.name) ^ CallStatement,

    simple_statement=Or(A.null_statement, A.assignment_statement,
                        A.goto_statement, A.exit_statement,
                        A.return_statement, A.requeue_statement,
                        A.call_statement, A.abort_statement, A.delay_statement,
                        A.raise_statement, A.terminate_statement, A.pragma),

    null_statement=A.null_literal ^ NullStatement,

    assignment_statement=Row(A.name, ":=", A.expression) ^ AssignStatement,

    goto_statement=Row("goto", A.static_name) ^ GotoStatement,

    exit_statement=Row("exit", Opt(A.identifier),
                       Opt("when", A.expression)[1]) ^ ExitStatement,

    return_statement=(
        Row("return", Opt(A.expression | A.raise_statement)) ^ ReturnStatement
    ),

    requeue_statement=Row(
        "requeue", A.expression, Opt("with", "abort").as_bool()
    ) ^ RequeueStatement,

    identifier=Tok(Token.Identifier, keep=True) ^ Identifier,
    enum_identifier=Tok(Token.Identifier, keep=True) ^ EnumIdentifier,
    char_literal=Tok(Token.Char, keep=True) ^ CharLiteral,
    string_literal=Tok(Token.String, keep=True) ^ StringLiteral,
    num_literal=Tok(Token.Number, keep=True) ^ NumLiteral,
    null_literal=Tok(Token.Null, keep=True) ^ NullLiteral,

    allocator=Row(
        "new", Opt("(", A.name, ")")[1], A.type_expression | A.name
    ) ^ Allocator,

    for_loop_parameter_spec=Row(
        A.identifier,
        Or(Enum("in", IterType("in")), Enum("of", IterType("of"))),
        Opt("reverse").as_bool(),
        A.constrained_type_ref | A.discrete_range | A.expression
    ) ^ ForLoopSpec,

    quantified_expression=Row(
        "for", Or(Enum("all", Quantifier("all")),
                  Enum("some", Quantifier("some"))),
        A.for_loop_parameter_spec, "=>",
        A.expression | A.discrete_range
    ) ^ QuantifiedExpr,

    attribute=Or(
        Or("access", "delta", "digits", "mod", "range") ^ Attribute,
        A.identifier
    ),

    case_expression=Row(
        "case", A.expression, "is",
        List(A.case_expr_alt, sep=",")
    ) ^ CaseExpr,

    case_expr_alt=Row(
        "when", A.choice_list, "=>", A.expression
    ) ^ CaseExprAlternative,


    raise_expression=Or(
        Row("raise", A.name, Opt("with", A.expression)[1]) ^ RaiseExpression,
        Row("raise", Null(Expr), Null(Expr)) ^ RaiseExpression,
    ),

    if_expression=Row(
        "if", A.expression, "then", A.expression,
        List(Row("elsif", A.expression,
                 "then", A.expression) ^ ElsifExprPart, empty_valid=True),
        Opt("else", A.expression)[1],
    ) ^ IfExpr,

    conditional_expression=Or(A.if_expression, A.case_expression,
                              A.quantified_expression),

    diamond_expr=Tok("<>") ^ DiamondExpr,

    others_designator=Tok("others") ^ OthersDesignator,

    aggregate_field=Or(
        A.choice_list ^ AggregateMember,
        A.expression,
        A.others_designator,
    ),

    aggregate_assoc=Row(
        Opt(A.aggregate_field, "=>")[0],
        Or(A.diamond_expr, A.expression)
    ) ^ AggregateAssoc,
    aggregate_content=List(A.aggregate_assoc, sep=",") ^ AggregateContent,
    aggregate_content_null=Row(
        "null", "record", Null(AggregateContent)
    )[2],

    aggregate=Row(
        "(",
        Row(
            Opt(A.expression, "with")[0],
            Or(A.aggregate_content_null, A.aggregate_content)
        ) ^ Aggregate,
        ")")[1],

    direct_name=Or(A.identifier, A.string_literal, A.char_literal,
                   A.access_deref, A.attribute),

    param_assoc=Row(
        Opt(A.identifier | A.others_designator | A.string_literal,
            "=>")[0],
        A.expression | A.diamond_expr
    ) ^ ParamAssoc,

    call_suffix=Or(
        A.constrained_type_ref,
        A.discrete_range,
        List(A.param_assoc, sep=",")
        ^ ParamList
    ),

    name=Or(
        Row(A.name, "(", A.call_suffix, ")") ^ CallExpr,
        Row(A.name, ".", A.direct_name) ^ Prefix,
        Row(A.name, "'", A.attribute,
            Opt("(", A.call_suffix, ")")[1]) ^ AttributeRef,
        Row(A.name, "'",
            Or(Row("(", A.expression, ")")[1], A.aggregate)) ^ QualExpr,
        A.direct_name,
    ),

    access_deref=Tok("all") ^ AccessDeref,

    static_name=List(
        # We want to accept string literals here for the corner case of library
        # child unit subprogram operators, such as:
        # procedure Ada.Containers.Vector."=" is ...
        A.identifier | A.string_literal | A.char_literal,
        sep=".", revtree=Prefix
    ),

    primary=Or(A.num_literal, A.null_literal,
               A.name, A.allocator,
               A.conditional_expression,
               A.raise_expression,
               Row("(", A.conditional_expression | A.expression, ")")[1],
               A.aggregate),

    factor=Or(
        Row(Or(Enum("abs", Op("abs")), Enum("not", Op("not"))),
            A.primary) ^ UnOp,

        Row(A.primary, Enum("**", Op("pow")), A.primary) ^ BinOp,

        A.primary
    ),

    term=Or(
        Row(A.factor, Or(Enum("*", Op("mult")),
                         Enum("/", Op("div")),
                         Enum("mod", Op("mod")),
                         Enum("rem", Op("rem"))), A.term) ^ BinOp,
        A.factor
    ),

    unop_term=Or(
        Row(Or(Enum("+", Op("plus")),
               Enum("-", Op("minus"))),
            A.term) ^ UnOp,
        A.term
    ),

    simple_expr=Or(
        Row(A.unop_term, Or(Enum("+", Op("plus")),
                            Enum("-", Op("minus")),
                            Enum("&", Op("bin_and"))),
            A.simple_expr_2) ^ BinOp,
        A.unop_term
    ),

    simple_expr_2=Or(
        Row(A.term, Or(Enum("+", Op("plus")),
                       Enum("-", Op("minus")),
                       Enum("&", Op("bin_and"))),
            A.simple_expr_2) ^ BinOp,
        A.term
    ),

    boolean_op=Or(
        Enum("xor", Op("xor")),
        Enum(Row("and", "then"), Op("and_then")), Enum("and", Op("and")),
        Enum(Row("or", "else"), Op("or_else")), Enum("or", Op("or")),
    ),

    discrete_range=Row(A.expression,
                       Enum("..", Op("ellipsis")), A.expression) ^ BinOp,

    choice=Or(
        A.constrained_type_ref, A.discrete_range, A.expression,
        A.others_designator
    ),

    choice_list=List(A.choice, sep="|"),

    rel_op=Or(
        Enum(Row("not", "in"), Op("not_in")),
        Enum("in", Op("in")),
    ),

    relation=Or(
        Row(A.simple_expr,
            Or(Enum("=", Op("eq")), Enum("/=", Op("neq")),
               Enum("<", Op("lt")), Enum("<=", Op("lte")),
               Enum(">", Op("gt")), Enum(">=", Op("gte"))),
            A.relation) ^ BinOp,

        Row(A.simple_expr, A.rel_op, A.choice_list) ^ MembershipExpr,

        A.simple_expr
    ),

    expression=Or(
        Row(A.relation, A.boolean_op, A.expression) ^ BinOp,
        A.relation
    ),
)
