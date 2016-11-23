#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_find.h"
#include "langkit_text.h"

struct my_unit_file_provider {
    int some_field;
};

void ufp_destroy(void *data) {
    struct my_unit_file_provider *ufp_data
      = (struct my_unit_file_provider *) data;
    printf("Calling ufp_destroy (some_field=%d)\n", ufp_data->some_field);
}

char *ufp_get_file_from_node(
    void *data,
    ada_base_node node,
    ada_unit_kind kind)
{
    struct my_unit_file_provider *ufp_data
      = (struct my_unit_file_provider *) data;

    char result_str[] = "strange_bar.ads";
    char *result = malloc(sizeof(result_str));
    strcpy(result, result_str);

    printf("Calling ufp_get_file_from_node (some_field=%d, kind=%d) on:\n",
           ufp_data->some_field, kind);
    dump_short_image(node, 0);

    return result;
}

char *ufp_get_file_from_name(
    void *data,
    ada_text name,
    ada_unit_kind kind)
{
    (void) data;
    (void) name;
    (void) kind;
    printf("Calling ufp_get_file_from_name: unsupported!\n");
    exit(1);
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;
    struct my_unit_file_provider ufp_data = { 42 };
    ada_unit_file_provider ufp
      = ada_create_unit_file_provider((void *) &ufp_data,
                                      ufp_destroy,
                                      ufp_get_file_from_node,
                                      ufp_get_file_from_name);

    ada_base_node pragma, args, assoc, expr;
    ada_ada_node_array entities;
    ada_text text;
    int i;

    libadalang_initialize();
    ctx = ada_create_analysis_context(NULL, ufp);
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0, 0);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");
    ada_unit_populate_lexical_env(unit);

    pragma = find_node(ada_unit_root(unit), ada_pragma_node);
    if (pragma == NULL)
      error("Could not find a PragmaNode node");
    if (!ada_pragma_node_f_args (pragma, &args) || args == NULL)
      error("Could not get PragmaNode.f_args");
    if (ada_node_child_count(args) != 1)
      error("PragmaNode.f_args should have exactly one child");
    if (!ada_node_child(args, 0, &assoc) || assoc == NULL)
      error("Could not get PragmaNode.f_args[0]");
    if (!ada_pragma_argument_assoc_f_expr(assoc, &expr) || expr == NULL)
      error("Could not get PragmaNode.f_args[0].f_expr");
    if (!ada_expr_p_entities(expr, &entities))
      error("Could not get PragmaNode.f_args[0].f_expr.p_entities");

    text = ada_node_short_image(expr);
    fprint_text(stdout, text, false);
    ada_destroy_text(&text);
    printf(" resolves to:\n");

    for (i = 0; i < entities->n; ++i) {
        ada_base_node ent = entities->items[i];

        printf("  ");
        text = ada_node_short_image(ent);
        fprint_text(stdout, text, false);
        ada_destroy_text(&text);
        printf("\n");
    }
    if (entities->n == 0)
      printf("  <nothing>\n");
    free(entities);

    ada_destroy_analysis_context(ctx);
    ada_destroy_unit_file_provider(ufp);
    puts("Done.");
    return 0;
}
