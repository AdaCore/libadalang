#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

#include "langkit_text.h"

#include "utils.h"


int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    ada_base_entity with_clause, subp_body, subp_name, has_limited,
                    has_private;
    ada_base_entity tmp;

    ada_bool is_limited, is_private;
    ada_base_entity overriding;
    ada_token tok;

    ctx = ada_allocate_analysis_context ();
    abort_on_exception ();

    ada_initialize_analysis_context (ctx, NULL, NULL, NULL, NULL, 1, 8);
    abort_on_exception ();

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0,
                                           ada_default_grammar_rule);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    ada_unit_root(unit, &tmp);
    if (ada_node_kind (&tmp) != ada_compilation_unit)
      error("Unit root is not a CompilationUnit");
    overriding.node = NULL;
    if (ada_base_subp_body_f_overriding(&tmp, &overriding))
      error("Getting CompilationUnit.overriding worked (this does not exist)");
    if (!ada_node_is_null(&overriding))
      error("Getting CompilationUnit.overriding failed but nevertheless output"
            " something");

    with_clause = tmp;
    if (ada_node_child(&tmp, 3, &tmp))
        error("ada_node_child returned a child that does not exist");
    tmp = with_clause;

    if (!ada_node_child(&tmp, 0, &tmp))
        error("Could not get CompilationUnit[0]");
    if (!ada_node_child(&tmp, 0, &tmp))
        error("Could not get CompilationUnit[0] -> list[0]");
    with_clause = tmp;

    if (ada_node_kind(&with_clause) != ada_with_clause)
        error("Got something else than a WithClause");
    if (!ada_with_clause_f_has_limited(&with_clause, &has_limited))
        error("Could got get WithClause.is_limited");
    if (!ada_with_clause_f_has_private(&with_clause, &has_private))
        error("Could got get WithClause.has_private");

    ada_limited_node_p_as_bool (&has_limited, &is_limited);
    ada_private_node_p_as_bool (&has_private, &is_private);

    printf("WithClause: is_limited = %s\n", is_limited ? "true" : "false");
    printf("WithClause: is_private = %s\n", is_private ? "true" : "false");


    ada_unit_root(unit, &tmp);
    if (!ada_node_child(&tmp, 1, &tmp))
        error("Could not get CompilationUnit[1]");
    if (!ada_node_child(&tmp, 1, &tmp))
        error("Could not get CompilationUnit[1] -> LibraryItem[1]");
    subp_body = tmp;

    if (ada_node_kind(&subp_body) != ada_subp_body)
        error("Got something else than a SubpBody");
    if (!ada_base_subp_body_f_overriding(&subp_body, &overriding))
        error("Could not get SubpBody.overriding");

    ada_text kind;
    ada_kind_name(ada_node_kind(&overriding), &kind);
    printf("SubpBody: overriding = ");
    fprint_text(stdout, kind, 0);
    printf("\n");


    if (!ada_base_subp_body_f_subp_spec(&subp_body, &tmp))
      error("Could not get SubpBody.subp_spec");
    if (ada_node_kind(&tmp) != ada_subp_spec)
      error("SubpBody.subp_spec is not a SubpSpec");

    if (!ada_subp_spec_f_subp_name(&tmp, &tmp))
      error("Could not get SubpBody.subp_spec.name");
    if (ada_node_kind(&tmp) != ada_defining_name)
      error("SubpBody.subp_spec.name is not a DefiningName");
    subp_name = tmp;

    if (!ada_ada_node_token_start(&subp_name, &tok))
      error("Could not get token for Identifier");
    printf("Identifier: tok = ");
    fprint_text(stderr, tok.text, false);
    putchar('\n');


    ada_context_decref(ctx);
    puts("Done.");
    return 0;
}
