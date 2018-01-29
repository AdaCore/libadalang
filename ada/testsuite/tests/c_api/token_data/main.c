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

    ada_base_entity node;
    ada_token tok;
    char *tk_name;

    libadalang_initialize();
    ctx = ada_create_analysis_context(NULL, 0, NULL);
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    ada_unit_root(unit, &node);
    if (ada_node_is_null(&node))
        error("Could not get analysis unit root node");
    if (!ada_compilation_unit_f_body(&node, &node))
        error("Could not get CompilationUnit.f_bodies");
    if (!ada_library_item_f_item(&node, &node))
        error("Could not get CompilationUnit.f_bodies[0].f_item");
    if (!ada_subp_body_f_subp_spec(&node, &node))
        error("Could not get CompilationUnit.f_bodies[0].f_item.f_subp_spec");
    if (!ada_subp_spec_f_subp_name(&node, &node))
        error("Could not get CompilationUnit.f_bodies[0].f_item.f_subp_spec"
              ".f_name");
    if (!ada_ada_node_token_start(&node, &tok))
        error("Could not get CompilationUnit.f_bodies[0].f_item.f_subp_spec"
              ".f_name.token_start");

    puts("Token data for the \"foo\" identifier:");
    tk_name = ada_token_kind_name(tok.kind);
    printf("Kind: %s\n", tk_name);
    free(tk_name);
    printf("Text: ");
    fprint_text(stdout, tok.text, false);
    printf("\n");

    printf("Sloc range: %u:%u-%u:%u\n",
       (unsigned) tok.sloc_range.start.line,
       (unsigned) tok.sloc_range.start.column,
       (unsigned) tok.sloc_range.end.line,
       (unsigned) tok.sloc_range.end.column);

    ada_destroy_analysis_context (ctx);

    puts("Done.");
    return 0;
}
