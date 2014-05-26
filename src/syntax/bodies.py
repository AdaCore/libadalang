from syntax import ASTNode, A, Field
from combinators import Opt, List, Or, Row, _, EnumType, Enum, TokClass
from syntax.exprs import SingleTokNode, LoopSpec
from tokenizer import Lbl


class CompilationUnit(ASTNode):
    fields = [
        Field("prelude", repr=True),
        Field("body", repr=True),
    ]


class SubprogramBody(ASTNode):
    fields = [
        Field("overriding", repr=True),
        Field("subp_spec", repr=True),
        Field("aspects", repr=True),
        Field("decls", repr=True),
        Field("statements", repr=True),
        Field("end_id", repr=True),
    ]


class HandledStatements(ASTNode):
    fields = [
        Field("statements", repr=True),
        Field("exceptions", repr=True)
    ]


class ExceptionHandler(ASTNode):
    fields = [
        Field("exc_name", repr=True),
        Field("catched_exceptions", repr=True),
        Field("statements", repr=True),
    ]


class StatementList(ASTNode):
    fields = [
        Field("statements", repr=True),
        Field("labels", repr=True)
    ]


class Statement(ASTNode):
    abstract = True


class StatementWithLabels(Statement):
    fields = [
        Field("labels", repr=True),
        Field("statement", repr=True)
    ]


class NullStatement(Statement):
    fields = [Field("null_lit")]


class AssignStatement(Statement):
    fields = [
        Field("dest", repr=True),
        Field("expr", repr=True),
    ]


class GotoStatement(Statement):
    fields = [Field("label_name", repr=True)]


class ExitStatement(Statement):
    fields = [
        Field("loop_name", repr=True),
        Field("condition", repr=True)
    ]


class ReturnStatement(Statement):
    fields = [
        Field("return_expr", repr=True)
    ]


class RequeueStatement(Statement):
    fields = [Field("call_name", repr=True),
              Field("with_abort", repr=True)]


class AbortStatement(Statement):
    fields = [Field("names", repr=True)]


class DelayStatement(Statement):
    fields = [
        Field("until", repr=True),
        Field("expr", repr=True)
    ]


class RaiseStatement(Statement):
    fields = [
        Field("exception_name", repr=True),
        Field("error_message", repr=True)
    ]


class IfStatement(Statement):
    fields = [
        Field("condition", repr=True),
        Field("statements", repr=True),
        Field("alternatives", repr=True),
        Field("else_statements", repr=True),
    ]


class Label(Statement):
    fields = [Field("token", repr=True)]


class WhileLoopSpec(LoopSpec):
    fields = [Field("expr", repr=True)]


class LoopStatement(Statement):
    fields = [
        Field("name", repr=True),
        Field("spec", repr=True),
        Field("statements", repr=True),
    ]


class BlockStatement(Statement):
    fields = [
        Field("name", repr=True),
        Field("decls", repr=True),
        Field("statements", repr=True),
    ]


class ExtReturnStatement(ASTNode):
    fields = [
        Field("object_decl", repr=True),
        Field("statements", repr=True),
    ]


class CaseStatement(Statement):
    fields = [
        Field("case_expr", repr=True),
        Field("case_alts", repr=True),
    ]


class AcceptStatement(Statement):
    fields = [
        Field("name", repr=True),
        Field("entry_index_expr", repr=True),
        Field("parameters", repr=True),
        Field("statements", repr=True),
    ]


class SelectStatement(Statement):
    fields = [
        Field("guards", repr=True),
        Field("else_statements", repr=True),
        Field("abort_statements", repr=True)
    ]


class TerminateStatement(Statement):
    fields = [Field("token")]


class PackageBody(ASTNode):
    fields = [
        Field("package_name", repr=True),
        Field("aspects", repr=True),
        Field("decls", repr=True),
        Field("statements", repr=True),
    ]


class TaskBody(ASTNode):
    fields = [
        Field("package_name", repr=True),
        Field("aspects", repr=True),
        Field("decls", repr=True),
        Field("statements", repr=True),
    ]


class ProtectedBody(ASTNode):
    fields = [
        Field("package_name", repr=True),
        Field("aspects", repr=True),
        Field("decls", repr=True),
        Field("body_stub", repr=True),
    ]


class EntryBody(ASTNode):
    fields = [
        Field("entry_name", repr=True),
        Field("index_spec", repr=True),
        Field("parameters", repr=True),
        Field("when_cond", repr=True),
        Field("decls", repr=True),
        Field("statements", repr=True),
    ]


class EntryIndexSpec(ASTNode):
    fields = [
        Field("id", repr=True),
        Field("subtype", repr=True),
    ]


class Subunit(ASTNode):
    fields = [
        Field("name", repr=True),
        Field("body", repr=True),
    ]


class BodyStub(ASTNode):
    fields = [
        Field("aspects", repr=True)
    ]


class SubprogramBodyStub(ASTNode):
    fields = [
        Field("overriding", repr=True),
        Field("subp_spec", repr=True),
        Field("aspects", repr=True)
    ]


class PackageBodyStub(ASTNode):
    fields = [
        Field("name", repr=True),
        Field("aspects", repr=True),
    ]


A.add_rules(
    subunit=Row(
        _("separate"), _("("), A.static_name, _(")"),
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

    library_item=Or(A.library_unit_body, A.library_unit_decl,
                    A.library_unit_renaming_decl),

    compilation_unit=Row(
        List(Row(A.context_item, ";") >> 0, empty_valid=True),
        Or(A.library_item, A.subunit)
    ) ^ CompilationUnit,

    entry_body=Row(
        _("entry"), A.identifier,
        Opt(Row(_("("), _("for"), A.identifier, _("in"),
                A.discrete_subtype_definition, _(")")) ^ EntryIndexSpec),
        Opt(A.parameter_profiles),
        _("when"), A.expression,
        _("is"), A.basic_decls,
        Opt("begin", A.handled_statements) >> 1,
        _("end"), _(Opt(A.static_name))
    ) ^ EntryBody,

    protected_body=Row(
        _("protected"), _("body"), A.static_name, A.aspect_specification,
        _("is"),
        Opt(A.basic_decls, "end", Opt(A.static_name)) >> 0,
        Opt((Row("separate", A.aspect_specification) >> 1) ^ BodyStub)
    ) ^ ProtectedBody,

    task_body=Row(
        _("task"), _("body"), A.static_name, A.aspect_specification,
        _("is"), A.basic_decls,
        Opt("begin", A.handled_statements) >> 1,
        _("end"), _(Opt(A.static_name))
    ) ^ TaskBody,

    package_body_stub=Row(
        _("package"), _("body"), A.static_name,
        _("is"), _("separate"), A.aspect_specification
    ) ^ PackageBodyStub,

    package_body=Row(
        _("package"), _("body"), A.static_name, A.aspect_specification,
        _("is"), A.basic_decls,
        Opt("begin", A.handled_statements) >> 1,
        _("end"), _(Opt(A.static_name))
    ) ^ PackageBody,

    terminate_statement=Row("terminate") ^ TerminateStatement,

    select_statement=Row(
        _("select"),
        List(Row(Opt("when", A.expression, "=>") >> 1, A.statements),
             sep="or"),
        Opt("else", A.statements) >> 1,
        Opt("then", "abort", A.statements) >> 2,
        _("end"), _("select")
    ) ^ SelectStatement,

    accept_statement=Row(
        _("accept"), A.identifier, Opt("(", A.expression, ")") >> 1,
        Opt(A.parameter_profiles),
        Opt("do", A.handled_statements, "end", Opt(A.identifier)) >> 1
    ) ^ AcceptStatement,

    case_alt=Row(
        _("when"), A.choice_list, _("=>"), A.statements
    ),

    case_statement=Row(
        _("case"), A.expression, _("is"), List(A.case_alt), _("end"), _("case")
    ) ^ CaseStatement,

    ext_return_statement=Row(
        _("return"), A._object_decl,
        Opt("do", A.handled_statements, "end", "return") >> 1
    ) ^ ExtReturnStatement,

    block_statement=Row(
        Opt(A.identifier, ":") >> 0,
        Opt("declare", A.basic_decls) >> 1,
        _("begin"), A.handled_statements, _("end"), _(Opt(A.identifier))
    ) ^ BlockStatement,

    loop_statement=Row(
        Opt(A.identifier, ":") >> 0,
        Opt(A.iteration_scheme),
        _("loop"),
        A.statements,
        _("end"), _("loop"), _(Opt(A.identifier))
    ) ^ LoopStatement,

    iteration_scheme=Or(
        Row("for", A.for_loop_parameter_spec) >> 1,
        Row(_("while"), A.expression) ^ WhileLoopSpec
    ),

    compound_statement=Or(A.if_statement, A.block_statement,
                          A.loop_statement, A.ext_return_statement,
                          A.case_statement, A.accept_statement,
                          A.select_statement),

    if_statement=Row(
        _("if"), A.expression, _("then"), A.statements,
        List(Row(_("elsif"), A.expression,
                 _("then"), A.statements), empty_valid=True),
        Opt("else", A.statements) >> 1,
        _("end"), _("if")
    ) ^ IfStatement,

    raise_statement=Row(
        _("raise"), Opt(A.name), Opt("with", A.expression) >> 1
    ) ^ RaiseStatement,

    delay_statement=Row(
        _("delay"), Opt("until").as_bool(), A.expression
    ) ^ DelayStatement,

    abort_statement=Row(
        _("abort"), List(A.static_name, sep=",")
    ) ^ AbortStatement,

    body=Or(A.subprogram_body, A.package_body, A.task_body,
            A.protected_body, A.entry_body),

    body_stub=Or(A.subprogram_body_stub, A.package_body_stub),

    subprogram_body_stub=Row(
        A.overriding_indicator,
        A.subprogram_spec,
        _("is"),
        _("separate"),
        A.aspect_specification
    ) ^ SubprogramBodyStub,

    subprogram_body=Row(A.overriding_indicator,
                        A.subprogram_spec,
                        A.aspect_specification,
                        _("is"),
                        A.basic_decls,
                        _("begin"),
                        A.handled_statements,
                        _("end"),
                        Opt(A.name)) ^ SubprogramBody,

    handled_statements=Row(
        A.statements, Opt("exception", List(A.exception_handler)) >> 1
    ) ^ HandledStatements,

    exception_handler=Row(
        _("when"), Opt(A.identifier, ":") >> 0,
        List(A.name | A.others_designator, sep="|"), _("=>"),
        A.statements
    ) ^ ExceptionHandler,

    statements=List(Or(Row(A.statement, ";") >> 0, A.label), empty_valid=True),

    label=TokClass(Lbl) ^ Label,

    statement=Or(A.compound_statement, A.simple_statement),

    simple_statement=Or(A.null_statement, A.assignment_statement,
                        A.goto_statement, A.exit_statement,
                        A.return_statement, A.requeue_statement,
                        A.name, A.abort_statement, A.delay_statement,
                        A.raise_statement, A.terminate_statement, A.pragma),

    null_statement=A.null_literal ^ NullStatement,

    assignment_statement=Row(A.name, _(":="), A.expression) ^ AssignStatement,

    goto_statement=Row(_("goto"), A.identifier) ^ GotoStatement,

    exit_statement=Row(_("exit"), Opt(A.identifier),
                       Opt("when", A.expression) >> 1) ^ ExitStatement,

    return_statement=Row(_("return"), Opt(A.expression | A.raise_statement)) ^
                     ReturnStatement,

    requeue_statement=Row(
        _("requeue"), A.static_name, Opt("with", "abort").as_bool()
    ) ^ RequeueStatement,
)
