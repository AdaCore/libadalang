#ifndef UNICODE_UTILS_H
#define UNICODE_UTILS_H

#include "libadalang.h"

#include "utils.h"


/* The following sources do not contain the same string literals so that
   testcases can check the the reparsing actually worked.  */

static const char *src_buffer_iso_8859_1 = (
    "with Ada.Text_IO; use Ada.Text_IO;\n"
    "\n"
    "procedure Test is\n"
    "begin\n"
    "   Put_Line(\"H\xe9llo w\xf6rld!\");\n"
    "end Test;\n"
);


static const char *src_buffer_utf_8 = (
    "with Ada.Text_IO; use Ada.Text_IO;\n"
    "\n"
    "procedure Test is\n"
    "begin\n"
    "   Put_Line(\"H\xc3\xa8llo w\xc3\xb5rld!\");\n"
    "end Test;\n"
);

/* Assming UNIT is one of the above source that is parsed successfuly, return
   the text associated to the string literal in the Put_Line call.  Exit if
   anything is unexpected (no AST, missing node, etc.).  */

static ada_text
get_string_literal(ada_analysis_unit unit) {
    ada_base_node node;
    ada_token tok;

    node = ada_unit_root(unit);
    if (node == NULL
        || ada_node_kind(node) != ada_compilation_unit)
        error("Got unexpected node for ada_unit_root [1]");
    if (!ada_compilation_unit_f_body(node, &node)
        || ada_node_kind(node) != ada_library_item)
        error("Got unexpected node for ada_compilation_unit_f_body [2]");
    if (!ada_library_item_f_item(node, &node)
        || ada_node_kind(node) != ada_subprogram_body)
        error("Got unexpected node for ada_library_item_f_item [4]");
    if (!ada_subprogram_body_f_statements(node, &node)
        || ada_node_kind(node) != ada_handled_statements)
        error("Got unexpected node for ada_subprogram_body_f_statements [5]");
    if (!ada_handled_statements_f_statements(node, &node)
        || ada_node_kind(node) != ada_list)
        error("Got unexpected node for"
              " ada_handled_statements_f_statements [6]");
    if (!ada_node_child(node, 0, &node)
        || ada_node_kind(node) != ada_call_statement)
        error("Got unexpected node for ada_node_child [7]");
    if (!ada_call_statement_f_call(node, &node)
        || ada_node_kind(node) != ada_call_expr)
        error("Got unexpected node for ada_call_statement_f_call [8]");
    if (!ada_call_expr_f_suffix(node, &node)
        || ada_node_kind(node) != ada_param_list)
        error("Got unexpected node for ada_call_expr_f_suffix [9]");
    if (!ada_param_list_f_params(node, &node)
        || ada_node_kind(node) != ada_list)
        error("Got unexpected node for ada_param_list_f_params [10]");
    if (!ada_node_child(node, 0, &node)
        || ada_node_kind(node) != ada_param_assoc)
        error("Got unexpected node for ada_node_child [11]");
    if (!ada_param_assoc_f_expr(node, &node)
        || ada_node_kind(node) != ada_string_literal)
        error("Got unexpected node for ada_param_assoc_f_expr [12]");

    if (!ada_single_tok_node_f_tok(node, &tok))
        error("Could not get token for the string literal");

    return ada_token_text(tok);
}

#endif /* UNICODE_UTILS_H */
