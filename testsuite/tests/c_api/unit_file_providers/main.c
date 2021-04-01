#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_find.h"
#include "langkit_text.h"

struct my_unit_provider {
    int some_field;
};

void ufp_destroy(void *data) {
    struct my_unit_provider *ufp_data
      = (struct my_unit_provider *) data;
    printf("Calling ufp_destroy (some_field=%d)\n", ufp_data->some_field);
}

char *ufp_get_unit_filename(
    void *data,
    ada_text *name,
    ada_analysis_unit_kind kind)
{
    const char result_static[] = "strange_bar.ads";
    char *result;
    struct my_unit_provider *ufp_data
      = (struct my_unit_provider *) data;

    printf("Calling ufp_get_unit_filename (some_field=%d, kind=%d) "
           "with name: \"", ufp_data->some_field, kind);
    fprint_text(stdout, *name, false);
    printf("\"\n");

    result = malloc(sizeof(result_static));
    strcpy(result, result_static);
    return result;
}

ada_analysis_unit ufp_get_file_from_name(
    void *data,
    ada_analysis_context context,
    ada_text *name,
    ada_analysis_unit_kind kind,
    const char *charset,
    int reparse)
{
    struct my_unit_provider *ufp_data
      = (struct my_unit_provider *) data;

    printf("Calling ufp_get_file_from_name (some_field=%d, kind=%d) "
           "with name: \"", ufp_data->some_field, kind);
    fprint_text(stdout, *name, false);
    printf("\"\n");

    return ada_get_analysis_unit_from_file(context, "strange_bar.ads",
                                           charset, reparse,
                                           ada_default_grammar_rule);
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;
    struct my_unit_provider ufp_data = { 42 };
    ada_unit_provider ufp
      = ada_create_unit_provider((void *) &ufp_data,
                                 ufp_destroy,
                                 ufp_get_unit_filename,
                                 ufp_get_file_from_name);

    ada_base_entity root, pragma, args, assoc, expr;
    ada_ada_node_array entities;
    ada_text text;
    int i;

    ctx = ada_create_analysis_context(NULL, NULL, ufp, 1, 8);
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0,
                                           ada_default_grammar_rule);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    ada_unit_root(unit, &root);
    find_node(&root, ada_pragma_node, &pragma);
    if (ada_node_is_null(&pragma))
      error("Could not find a PragmaNode node");
    if (!ada_pragma_node_f_args (&pragma, &args)
        || ada_node_is_null(&args))
      error("Could not get PragmaNode.f_args");
    if (ada_node_children_count(&args) != 1)
      error("PragmaNode.f_args should have exactly one child");
    if (!ada_node_child(&args, 0, &assoc) || ada_node_is_null(&assoc))
      error("Could not get PragmaNode.f_args[0]");
    if (!ada_pragma_argument_assoc_f_expr(&assoc, &expr)
        || ada_node_is_null(&expr))
      error("Could not get PragmaNode.f_args[0].f_expr");
    if (!ada_expr_p_matching_nodes(&expr, &entities))
      error("Could not get PragmaNode.f_args[0].f_expr.p_matching_nodes");

    ada_node_image(&expr, &text);
    fprint_text(stdout, text, false);
    ada_destroy_text(&text);
    printf(" resolves to:\n");

    for (i = 0; i < entities->n; ++i) {
        ada_base_entity *ent = &entities->items[i];

        printf("  ");
        ada_node_image(ent, &text);
        fprint_text(stdout, text, false);
        ada_destroy_text(&text);
        printf("\n");
    }
    if (entities->n == 0)
      printf("  <nothing>\n");
    ada_ada_node_array_dec_ref(entities);

    ada_context_decref(ctx);
    ada_dec_ref_unit_provider(ufp);
    puts("Done.");
    return 0;
}
