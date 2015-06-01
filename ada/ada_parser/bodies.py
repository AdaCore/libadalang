from parsers import Opt, List, Or, Row, _, TokClass, Null
from compiled_types import NoToken, Field, abstract, ASTNode
from ada_parser.exprs import LoopSpec, Expr
from ada_parser import A
from tokenizer import Lbl


class CompilationUnit(ASTNode):
    prelude = Field()
    bodies = Field()


class SubprogramBody(ASTNode):
    overriding = Field()
    subp_spec = Field()
    aspects = Field()
    decls = Field(indent=3)
    statements = Field(indent=3)
    end_id = Field()


class HandledStatements(ASTNode):
    statements = Field()
    exceptions = Field()


class ExceptionHandler(ASTNode):
    exc_name = Field()
    catched_exceptions = Field()
    statements = Field()


class StatementList(ASTNode):
    statements = Field()
    labels = Field()


@abstract
class Statement(ASTNode):
    pass


class StatementWithLabels(Statement):
    labels = Field()
    statement = Field()


class NullStatement(Statement):
    null_lit = Field(repr=False)


class AssignStatement(Statement):
    dest = Field()
    expr = Field()


class GotoStatement(Statement):
    label_name = Field()


class ExitStatement(Statement):
    loop_name = Field()
    condition = Field()


class ReturnStatement(Statement):
    return_expr = Field()


class RequeueStatement(Statement):
    call_name = Field()
    with_abort = Field()


class AbortStatement(Statement):
    names = Field()


class DelayStatement(Statement):
    until = Field()
    expr = Field()


class RaiseStatement(Statement):
    exception_name = Field()
    error_message = Field()


class IfStatement(Statement):
    condition = Field()
    statements = Field(indent=3)
    alternatives = Field()
    else_statements = Field(indent=3)


class ElsifStatementPart(ASTNode):
    expr = Field()
    statements = Field(indent=3)


class Label(Statement):
    token = Field()


class WhileLoopSpec(LoopSpec):
    expr = Field()


class LoopStatement(Statement):
    name = Field()
    spec = Field()
    statements = Field()


class BlockStatement(Statement):
    name = Field()
    decls = Field()
    statements = Field()


class ExtReturnStatement(ASTNode):
    object_decl = Field()
    statements = Field()


class CaseStatement(Statement):
    case_expr = Field()
    case_alts = Field()


class CaseStatementAlternative(ASTNode):
    choices = Field()
    statements = Field()


class AcceptStatement(Statement):
    name = Field()
    entry_index_expr = Field()
    parameters = Field()
    statements = Field()


class SelectStatement(Statement):
    guards = Field()
    else_statements = Field()
    abort_statements = Field()


class SelectWhenPart(Statement):
    choices = Field()
    statements = Field()


class TerminateStatement(Statement):
    pass


class PackageBody(ASTNode):
    package_name = Field()
    aspects = Field()
    decls = Field(indent=3)
    statements = Field(indent=3)


class TaskBody(ASTNode):
    package_name = Field()
    aspects = Field()
    decls = Field()
    statements = Field()


class ProtectedBody(ASTNode):
    package_name = Field()
    aspects = Field()
    decls = Field()
    body_stub = Field()


class EntryBody(ASTNode):
    entry_name = Field()
    index_spec = Field()
    parameters = Field()
    when_cond = Field()
    decls = Field()
    statements = Field()


class EntryIndexSpec(ASTNode):
    id = Field()
    subtype = Field()


class Subunit(ASTNode):
    name = Field()
    body = Field()


class BodyStub(ASTNode):
    aspects = Field()


class SubprogramBodyStub(ASTNode):
    overriding = Field()
    subp_spec = Field()
    aspects = Field()


class PackageBodyStub(ASTNode):
    name = Field()
    aspects = Field()


class LibraryItem(ASTNode):
    is_private = Field()
    item = Field()


A.add_rules(
    subunit=Row(
        "separate", "(", A.static_name, ")",
        Or(A.subprogram_body, A.package_body, A.task_body, A.protected_body)
    ) ^ Subunit,

    library_unit_body=Or(
        A.subprogram_body, A.package_body
    ),

    library_unit_decl=Or(
        A.subprogram_decl, A.generic_decl, A.package_decl,
        A.generic_instantiation
    ),

    library_unit_renaming_decl=Or(
        A.package_renaming_decl,
        A.generic_renaming_decl,
    ),

    library_item=Row(
        Opt("private").as_bool(),
        Or(A.library_unit_body, A.library_unit_decl,
           A.library_unit_renaming_decl)
    ) ^ LibraryItem,

    compilation_unit=Row(
        List(Row(A.context_item, ";") >> 0, empty_valid=True),
        List(Row(A.library_item | A.subunit |
                 A.generic_instantiation | A.pragma, ";") >> 0,
             empty_valid=True),
    ) ^ CompilationUnit,

    entry_body=Row(
        "entry", A.identifier,
        Opt(Row("(", "for", A.identifier, "in",
                A.discrete_subtype_definition, ")") ^ EntryIndexSpec),
        Opt(A.parameter_profiles),
        "when", A.expression,
        "is", A.basic_decls,
        Opt("begin", A.handled_statements) >> 1,
        "end", _(Opt(A.static_name))
    ) ^ EntryBody,

    protected_body=Row(
        "protected", "body", A.static_name, A.aspect_specification,
        "is",
        Opt(A.basic_decls, "end", Opt(A.static_name)) >> 0,
        Opt((Row("separate", A.aspect_specification) >> 1) ^ BodyStub)
    ) ^ ProtectedBody,

    task_body=Row(
        "task", "body", A.static_name, A.aspect_specification,
        "is", A.basic_decls,
        Opt("begin", A.handled_statements) >> 1,
        "end", _(Opt(A.static_name))
    ) ^ TaskBody,

    package_body_stub=Row(
        "package", "body", A.static_name,
        "is", "separate", A.aspect_specification
    ) ^ PackageBodyStub,

    package_body=Row(
        "package", "body", A.static_name, A.aspect_specification,
        "is", A.basic_decls,
        Opt("begin", A.handled_statements) >> 1,
        "end", _(Opt(A.static_name))
    ) ^ PackageBody,

    terminate_statement=Row("terminate") ^ TerminateStatement,

    select_statement=Row(
        "select",
        List(
            Row(Opt("when", A.expression, "=>") >> 1,
                A.statements) ^ SelectWhenPart,
            sep="or"),
        Opt("else", A.statements) >> 1,
        Opt("then", "abort", A.statements) >> 2,
        "end", "select"
    ) ^ SelectStatement,

    accept_statement=Row(
        "accept", A.identifier, Opt("(", A.expression, ")") >> 1,
        Opt(A.parameter_profiles),
        Opt("do", A.handled_statements, "end", Opt(A.identifier)) >> 1
    ) ^ AcceptStatement,

    case_alt=Row(
        "when", A.choice_list, "=>", A.statements
    ) ^ CaseStatementAlternative,

    case_statement=Row(
        "case", A.expression, "is", List(A.case_alt), "end", "case"
    ) ^ CaseStatement,

    ext_return_statement=Row(
        "return", A.sub_object_decl,
        Opt("do", A.handled_statements, "end", "return") >> 1
    ) ^ ExtReturnStatement,

    block_statement=Row(
        Opt(A.identifier, ":") >> 0,
        Opt("declare", A.basic_decls) >> 1,
        "begin", A.handled_statements, "end", _(Opt(A.identifier))
    ) ^ BlockStatement,

    loop_statement=Row(
        Opt(A.identifier, ":") >> 0,
        Opt(A.iteration_scheme),
        "loop",
        A.statements,
        "end", "loop", _(Opt(A.identifier))
    ) ^ LoopStatement,

    iteration_scheme=Or(
        Row("for", A.for_loop_parameter_spec) >> 1,
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
        Opt("else", A.statements) >> 1,
        "end", "if"
    ) ^ IfStatement,

    raise_statement=Or(
        Row("raise", A.name, Opt("with", A.expression) >> 1) ^ RaiseStatement,
        Row("raise", Null(Expr), Null(Expr)) ^ RaiseStatement,
    ),

    delay_statement=Row(
        "delay", Opt("until").as_bool(), A.expression
    ) ^ DelayStatement,

    abort_statement=Row(
        "abort", List(A.static_name, sep=",")
    ) ^ AbortStatement,

    body=Or(A.subprogram_body, A.package_body, A.task_body,
            A.protected_body, A.entry_body),

    body_stub=Or(A.subprogram_body_stub, A.package_body_stub),

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
        A.statements, Opt("exception", List(A.exception_handler)) >> 1
    ) ^ HandledStatements,

    exception_handler=Row(
        "when", Opt(A.identifier, ":") >> 0,
        List(A.name | A.others_designator, sep="|"), "=>",
        A.statements
    ) ^ ExceptionHandler,

    statements=List(Or(Row(A.statement, Opt(";").error()) >> 0,
                       A.label), empty_valid=True),

    label=TokClass(Lbl) ^ Label,

    statement=Or(A.compound_statement, A.simple_statement),

    simple_statement=Or(A.null_statement, A.assignment_statement,
                        A.goto_statement, A.exit_statement,
                        A.return_statement, A.requeue_statement,
                        A.name, A.abort_statement, A.delay_statement,
                        A.raise_statement, A.terminate_statement, A.pragma),

    null_statement=A.null_literal ^ NullStatement,

    assignment_statement=Row(A.name, ":=", A.expression) ^ AssignStatement,

    goto_statement=Row("goto", A.identifier) ^ GotoStatement,

    exit_statement=Row("exit", Opt(A.identifier),
                       Opt("when", A.expression) >> 1) ^ ExitStatement,

    return_statement=(
        Row("return", Opt(A.expression | A.raise_statement)) ^ ReturnStatement
    ),

    requeue_statement=Row(
        "requeue", A.expression, Opt("with", "abort").as_bool()
    ) ^ RequeueStatement,
)
