#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"


static void
error(const char *msg)
{
    fputs(msg, stderr);
    exit(1);
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    ada_node with_decl, subp_body, subp_name;
    ada_node tmp;

    int is_limited, is_private;
    enum ada_Overriding overriding;
    ada_token tok;

    ctx = ada_create_analysis_context();
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_create_analysis_unit_from_file(ctx, "foo.adb");
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");


    tmp = ada_unit_root(unit);
    if (ada_node_kind (tmp) != ada_CompilationUnit)
      error("Unit root is not a CompilationUnit");
    overriding = 100;
    if (ada_SubprogramBody_overriding(tmp, &overriding))
      error("Getting CompilationUnit.overriding worked (this does not exist)");
    if (overriding != 100)
      error("Getting CompilationUnit.overriding failed but nevertheless output"
            " something");


    with_decl = tmp;
    if (ada_node_child(tmp, 2, &tmp))
        error("ada_node_child returned a child that does not exist");
    if (tmp != with_decl)
        error("ada_node_child failed but nevertheless output something");
    tmp = with_decl;

    if (!ada_node_child(tmp, 0, &tmp))
        error("Could not get CompilationUnit[0]");
    if (!ada_node_child(tmp, 0, &tmp))
        error("Could not get CompilationUnit[0] -> list[0]");
    with_decl = tmp;

    if (ada_node_kind(with_decl) != ada_WithDecl)
        error("Got something else than a WithDecl");
    if (!ada_WithDecl_is_limited(with_decl, &is_limited))
        error("Could got get WithDecl.is_limited");
    if (!ada_WithDecl_is_private(with_decl, &is_private))
        error("Could got get WithDecl.is_private");
    printf ("WithDecl: is_limited = %s\n", is_limited ? "true" : "false");
    printf ("WithDecl: is_private = %s\n", is_private ? "true" : "false");


    tmp = ada_unit_root(unit);
    if (!ada_node_child(tmp, 1, &tmp))
        error("Could not get CompilationUnit[1]");
    if (!ada_node_child(tmp, 0, &tmp))
        error("Could not get CompilationUnit[1] -> LibraryItem[0]");
    subp_body = tmp;

    if (ada_node_kind(subp_body) != ada_SubprogramBody)
        error("Got something else than a SubprogramBody");
    if (!ada_SubprogramBody_overriding(subp_body, &overriding))
        error("Could got get SubprogramBody.overriding");
    printf ("SubprogramBody: overriding = %d\n", overriding);


    if (!ada_SubprogramBody_subp_spec(subp_body, &tmp))
      error("Could not get SubprogramBody.subp_spec");
    if (ada_node_kind(tmp) != ada_SubprogramSpec)
      error("SubprogramBody.subp_spec is not a SubprogramSpec");

    if (!ada_SubprogramSpec_name(tmp, &tmp))
      error("Could not get SubprogramBody.subp_spec.name");
    if (ada_node_kind(tmp) != ada_Identifier)
      error("SubprogramBody.subp_spec.name is not an Identifier");
    subp_name = tmp;

    if (!ada_Identifier_tok(subp_name, &tok))
      error("Could not get Identifier.tok");
    printf ("Identifier: tok = %s\n", ada_token_text(tok));


    ada_destroy_analysis_context(ctx);
    puts("Done.");
    return 0;
}
