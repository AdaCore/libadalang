#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"

#include "langkit_text.h"
#include "utils.h"


int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;
    ada_node node;
    ada_token tok;

    libadalang_initialize();
    ctx = ada_create_analysis_context();
    if (ctx == NULL)
        error("Could not create the analysis context\n");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", 0);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    node = ada_unit_root(unit);
    if (node == NULL
        || ada_node_kind(node) != ada_compilation_unit)
        error("Got unexpected node for ada_unit_root [1]");
    if (!ada_compilation_unit_f_bodies(node, &node)
        || ada_node_kind(node) != ada_list)
        error("Got unexpected node for ada_compilation_unit_f_bodies [2]");
    if (!ada_node_child(node, 0, &node)
        || ada_node_kind(node) != ada_library_item)
        error("Got unexpected node for ada_node_child [3]");
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
        || ada_node_kind(node) != ada_call_expr)
        error("Got unexpected node for ada_node_child [7]");
    if (!ada_call_expr_f_suffix(node, &node)
        || ada_node_kind(node) != ada_param_list)
        error("Got unexpected node for ada_call_expr_f_suffix [8]");
    if (!ada_param_list_f_params(node, &node)
        || ada_node_kind(node) != ada_list)
        error("Got unexpected node for ada_param_list_f_params [9]");
    if (!ada_node_child(node, 0, &node)
        || ada_node_kind(node) != ada_param_assoc)
        error("Got unexpected node for ada_node_child [10]");
    if (!ada_param_assoc_f_expr(node, &node)
        || ada_node_kind(node) != ada_string_literal)
        error("Got unexpected node for ada_param_assoc_f_expr [11]");

    if (!ada_single_tok_node_f_tok(node, &tok))
        error("Could not get token for the string literal");

    printf("Here's the string literal we parsed:\n");
    fprint_text(stdout, ada_token_text(tok), false);
    putchar('\n');

    ada_destroy_analysis_context(ctx);
    puts("Done");
    return 0;
}
