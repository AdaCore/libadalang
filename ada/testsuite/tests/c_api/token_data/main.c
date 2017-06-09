#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"

#include "langkit_text.h"


static void
error(const char *msg)
{
    const ada_exception *exc = ada_get_last_exception();

    fputs(msg, stderr);
    if (exc != NULL) {
        puts("Last Ada exception:");
        puts(exc->information);
    }

    exit(1);
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    ada_base_node node;
    ada_token tok;
    char *tk_name;

    libadalang_initialize();
    ctx = ada_create_analysis_context(NULL, NULL);
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0, 0);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    node = ada_unit_root(unit);
    if (node == NULL)
        error("Could not get analysis unit root node");
    if (!ada_compilation_unit_f_body(node, &node))
        error("Could not get CompilationUnit.f_bodies");
    if (!ada_library_item_f_item(node, &node))
        error("Could not get CompilationUnit.f_bodies[0].f_item");
    if (!ada_subp_body_f_subp_spec(node, &node))
        error("Could not get CompilationUnit.f_bodies[0].f_item.f_subp_spec");
    if (!ada_subp_spec_f_subp_name(node, &node))
        error("Could not get CompilationUnit.f_bodies[0].f_item.f_subp_spec"
              ".f_name");
    if (!ada_single_tok_node_f_tok(node, &tok))
        error("Could not get CompilationUnit.f_bodies[0].f_item.f_subp_spec"
              ".f_name.f_tok");

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
