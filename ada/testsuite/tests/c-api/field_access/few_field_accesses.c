#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"

#include "langkit_text.h"


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

    ada_base_node with_decl, subp_body, subp_name;
    ada_base_node tmp;

    int is_limited, is_private;
    ada_overriding overriding;
    ada_token tok;
    ada_text tok_text;

    libadalang_initialize();
    ctx = ada_create_analysis_context("iso-8859-1");
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");


    tmp = ada_unit_root(unit);
    if (ada_node_kind (tmp) != ada_compilation_unit)
      error("Unit root is not a CompilationUnit");
    overriding = 100;
    if (ada_subprogram_body_f_overriding(tmp, &overriding))
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

    if (ada_node_kind(with_decl) != ada_with_decl)
        error("Got something else than a WithDecl");
    if (!ada_with_decl_f_is_limited(with_decl, &is_limited))
        error("Could got get WithDecl.is_limited");
    if (!ada_with_decl_f_is_private(with_decl, &is_private))
        error("Could got get WithDecl.is_private");
    printf("WithDecl: is_limited = %s\n", is_limited ? "true" : "false");
    printf("WithDecl: is_private = %s\n", is_private ? "true" : "false");


    tmp = ada_unit_root(unit);
    if (!ada_node_child(tmp, 1, &tmp))
        error("Could not get CompilationUnit[1]");
    if (!ada_node_child(tmp, 0, &tmp))
        error("Could not get CompilationUnit[1] -> Bodies[0]");
    if (!ada_node_child(tmp, 0, &tmp))
        error("Could not get CompilationUnit[1] -> Bodies[0] -> LibraryItem[0]");
    subp_body = tmp;

    if (ada_node_kind(subp_body) != ada_subprogram_body)
        error("Got something else than a SubprogramBody");
    if (!ada_subprogram_body_f_overriding(subp_body, &overriding))
        error("Could got get SubprogramBody.overriding");
    printf("SubprogramBody: overriding = %d\n", overriding);


    if (!ada_subprogram_body_f_subp_spec(subp_body, &tmp))
      error("Could not get SubprogramBody.subp_spec");
    if (ada_node_kind(tmp) != ada_subprogram_spec)
      error("SubprogramBody.subp_spec is not a SubprogramSpec");

    if (!ada_subprogram_spec_f_name(tmp, &tmp))
      error("Could not get SubprogramBody.subp_spec.name");
    if (ada_node_kind(tmp) != ada_identifier)
      error("SubprogramBody.subp_spec.name is not an Identifier");
    subp_name = tmp;

    if (!ada_identifier_f_tok(subp_name, &tok))
      error("Could not get Identifier.tok");
    tok_text = ada_token_text(tok);
    printf("Identifier: tok = ");
    fprint_text(stderr, tok_text, false);
    putchar('\n');


    ada_destroy_analysis_context(ctx);
    puts("Done.");
    return 0;
}
