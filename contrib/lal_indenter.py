"""
This is a toy/prototype indenter for Libadalang. Its purposes are:

1. Experimentation. This is used to help designing the proper primitives in
   Langkit so that it is easy to build a code indenter on top of it.

2. Demo. This is a good showcase of how to do an indenter on top of a Langkit
   library.

3. Prototype for a language agnostic indenter: Ultimately the logic in this
   code would go in Langkit, and the language specific annotations in a user
   defined file, so that people can specify how to indent their language and
   get an indenter for free.
"""

from collections import defaultdict
import libadalang as lal
import logging


logger = logging.getLogger()

#######################################################
# Definition of indentation rules classes and helpers #
#######################################################


class FieldIndentRules(object):
    def __init__(self, constant_increment=0):
        self.constant_increment = constant_increment

    def __repr__(self):
        return "<FieldIndentRules {}>".format(
            self.constant_increment
        )


class IndentRules(object):
    def __init__(self, field_rules=None, cont_line=None,
                 on_token_start=None, on_token_end=None):
        self._complete = False
        self.field_rules = field_rules or indent_fields()
        self._cont_line = cont_line
        self.on_token_start = on_token_start
        self.on_token_end = on_token_end

    def update(self, other):
        self.field_rules.update(other.field_rules)
        if self._cont_line is None:
            self._cont_line = other._cont_line

    @property
    def cont_line(self):
        return self._cont_line or 0

    def __repr__(self):
        return "<IndentRules {} {}>".format(
            self.field_rules, self.cont_line
        )


def indent_fields(**kwargs):
    return defaultdict(FieldIndentRules, kwargs)


def field_rules(**kwargs):
    return FieldIndentRules(**kwargs)

#########################################
# Libadalang specific indentation rules #
#########################################


block_rule = field_rules(constant_increment=3)
paren_rule = IndentRules(on_token_start="(", on_token_end=")")

indent_map = {
    lal.SubpBody: IndentRules(
        field_rules=indent_fields(decls=block_rule, stmts=block_rule)
    ),
    lal.PackageBody: IndentRules(
        field_rules=indent_fields(decls=block_rule, stmts=block_rule)
    ),
    lal.BasePackageDecl: IndentRules(
        field_rules=indent_fields(
            public_part=block_rule, private_part=block_rule
        )
    ),
    lal.BaseRecordDef: IndentRules(
        field_rules=indent_fields(components=block_rule)
    ),

    lal.Params: paren_rule,
    lal.ParenExpr: paren_rule,
    lal.Aggregate: paren_rule,
    lal.CallExpr: paren_rule,

    lal.Stmt: IndentRules(cont_line=2),
    lal.ObjectDecl: IndentRules(cont_line=2),
}

#################
# Indenter code #
#################


def get_indent_for_type(typ):
    irules = indent_map.get(typ, None)

    if not irules:
        irules = IndentRules()

    elif irules._complete:
        return irules

    if issubclass(typ.__base__, lal.AdaNode):
        irules.update(get_indent_for_type(typ.__base__))
        irules._complete = True

    indent_map[typ] = irules
    return irules


def indent_for_node(node, mmz_context=None):
    if mmz_context:
        if node in mmz_context:
            return mmz_context[node]

    parent_chain = node.parent_chain
    current_indent = 0

    while parent_chain:
        cur_node = parent_chain.pop()
        if cur_node == node:
            break

        indent_rules = get_indent_for_type(type(cur_node))
        indent_fields = indent_rules.field_rules
        for (field_name, child) in cur_node.iter_fields():
            if child == parent_chain[-1]:
                increment_indent = indent_fields[field_name[2:]]
                current_indent += increment_indent
                break

    if mmz_context:
        mmz_context[node] = current_indent

    return current_indent


def indent_for_line(line, buffer_text, unit, mmz_context=None):
    """
    """
    line_text = buffer_text[line - 1]
    start_col = 1
    for i, c in enumerate(line_text, 1):
        start_col = i
        if not c.isspace():
            break

    n = unit.root.lookup(lal.Sloc(line, start_col))
    return indent_for_node(n, mmz_context)


def indent_all_file(unit, buffer):
    """
    This function is the main function of the indenter. It will return a list,
    indexed by line numbers, which elements are the indentation in spaces for
    each line.

    :param lal.AnalysisUnit unit: The analysis unit to process.
    :param str buffer: The text of the unit.
    """
    indent_buffer = [[0, None]
                     for _ in range(0, unit.root.sloc_range.end.line + 1)]

    def indent_internal(node, increment=-1):

        if not isinstance(node, lal.AdaNode):
            return

        indent_rules = get_indent_for_type(type(node))
        fixed_level = -1

        logger.info("node: {}".format(node))
        logger.info("cont_line: {}".format(indent_rules.cont_line))

        if indent_buffer[node.sloc_range.start.line - 1][1] is None:
            indent_buffer[node.sloc_range.start.line - 1][1] = node

        startl = endl = -1
        if indent_rules.on_token_start:
            if increment > 0:
                logger.error(
                    "Increment = {}, but indent rules for {} have "
                    "on_token_start = {}".format(
                        increment, type(node), indent_rules.on_token_start
                    )
                )
                assert False

            for t in node.tokens:
                if t.text == indent_rules.on_token_start:
                    startl = t.sloc_range.start.line
                    fixed_level = t.sloc_range.start.column
                elif t.text == indent_rules.on_token_end:
                    endl = t.sloc_range.end.line

            if endl == -1:
                endl = node.sloc_range.end.line

            logger.info(
                "==========  In on_token mode, fixed level = {}"
                " start line = {} end line = {}".format(
                    fixed_level, startl, endl
                )
            )
        else:
            startl = node.sloc_range.start.line
            endl = node.sloc_range.end.line

        if startl == -1 or endl == -1:
            logger.error(
                "start line and end line not set. Consistency error !"
            )
            assert False

        next_tok = node.token_end.next
        prev_tok = node.token_start.previous

        # If there is another node on the start line, start one line below
        if prev_tok and prev_tok.sloc_range.end.line == startl:
            startl = startl + 1

        # If there is another node on the end line, start one line above
        if (next_tok
                and next_tok.sloc_range.start.line == endl
                and node.token_end.sloc_range.end.line != endl):
            endl = endl - 1

        logger.info(
            "==========  After adjusting "
            " start line = {} end line = {}".format(startl, endl)
        )

        for l in range(startl, endl + 1):
            logger.info("Node: {}, Line: {}".format(node, l))
            logger.info("Increment: {}".format(increment))
            owning_node = indent_buffer[l - 1][1]
            if owning_node is not None and owning_node != node:
                continue

            if fixed_level != -1:
                indent_buffer[l - 1][0] = fixed_level
            else:
                indent_buffer[l - 1][0] += increment

            if l != startl:
                indent_buffer[l - 1][0] += indent_rules.cont_line

        # Process fields

        indent_fields = indent_rules.field_rules
        for field_name, child in node.iter_fields():
            # print("Child: {}: {}".format(field_name, child))

            ir = indent_fields[field_name[2:]]
            logger.info("constant_increment: {}".format(ir.constant_increment))

            indent_internal(child, ir.constant_increment)

    indent_internal(unit.root, 0)

    return [i[0] for i in indent_buffer]
