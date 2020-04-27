#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_find.h"
#include "langkit_text.h"

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    uint32_t unit_name_chars[2] = { 'p', '2' };
    ada_text unit_name = { unit_name_chars, 2, true };

    ada_base_entity root, subtype_ind, name;
    ada_ada_node_array entities;
    ada_text text;
    int i;

    ctx = ada_create_analysis_context(NULL, NULL, 1, 8);
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_provider(
        ctx, &unit_name, ADA_ANALYSIS_UNIT_KIND_UNIT_SPECIFICATION, NULL, 0
    );
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    ada_unit_root(unit, &root);
    find_node(&root, ada_subtype_indication, &subtype_ind);
    if (ada_node_is_null(&subtype_ind))
      error("Could not find a SubtypeIndication node");
    if (!ada_subtype_indication_f_name (&subtype_ind, &name)
        || ada_node_is_null(&name))
      error("Could not get SubtypeIndication.f_name");
    if (!ada_expr_p_matching_nodes(&name, &entities))
      error("Could not get SubtypeIndication.f_name.p_matching_nodes");

    ada_node_short_image(&subtype_ind, &text);
    fprint_text(stdout, text, false);
    ada_destroy_text(&text);
    printf(" resolves to:\n");

    for (i = 0; i < entities->n; ++i) {
        ada_base_entity *ent = &entities->items[i];

        printf("  ");
        ada_node_short_image(ent, &text);
        fprint_text(stdout, text, false);
        ada_destroy_text(&text);
        printf("\n");
    }
    if (entities->n == 0)
      printf("  <nothing>\n");
    ada_ada_node_array_dec_ref(&entities);

    ada_context_decref(ctx);
    puts("Done.");
    return 0;
}
