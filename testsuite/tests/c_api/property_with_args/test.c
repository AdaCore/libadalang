#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"

#include "langkit_text.h"
#include "utils.h"

static void
dump_identifier(ada_base_node id)
{
    ada_text text;
    ada_token name;

    ada_kind_name(ada_node_kind(id), &text);
    printf("  ");
    fprint_text(stdout, text, 0);
    printf(": ");
    ada_destroy_text(&text);

    if (!ada_ada_node_token_start(id, &name))
      error("Could not the the name of an Identifier");
    ada_token_range_text (&name, &name, &text);
    fprint_text(stdout, text, 0);
    printf("\n");
    ada_destroy_text(&text);
}

static const char *
bool_image(ada_bool b)
{
  return b ? "True" : "False";
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    ada_base_entity foo, i;
    ada_base_entity tmp;
    ada_bool boolean;

    ctx = ada_allocate_analysis_context ();
    abort_on_exception ();

    ada_initialize_analysis_context (ctx, NULL, NULL, NULL, NULL, 1, 8);
    abort_on_exception ();

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0,
                                           ada_default_grammar_rule);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    ada_unit_root(unit, &tmp);
    if (ada_node_kind(&tmp) != ada_compilation_unit)
      error("Unit root is not a CompilationUnit");

    ada_unit_root(unit, &tmp);
    if (!ada_compilation_unit_f_body(&tmp, &tmp))
        error("Could not get CompilationUnit -> Body");
    if (!ada_library_item_f_item(&tmp, &tmp))
        error("Could not get CompilationUnit -> Body -> Item");
    if (!ada_base_subp_body_f_subp_spec(&tmp, &tmp))
        error("Could not get CompilationUnit -> Body -> Item ->"
	      " SubpSpec");

    if (!ada_subp_spec_f_subp_name(&tmp, &foo))
        error("Could not get CompilationUnit -> Body -> Item ->"
	      " SubpSpec -> Name");

    if (!ada_subp_spec_f_subp_params(&tmp, &tmp))
        error("Could not get CompilationUnit -> Body -> Item ->"
	      " SubpSpec -> Params");

    if (!ada_params_f_params(&tmp, &tmp))
        error("Could not get CompilationUnit -> Body -> Item ->"
	      " SubpSpec -> Params -> Params");

    if (!ada_node_child(&tmp, 0, &tmp))
        error("Could not get CompilationUnit -> Body -> Item ->"
	      " SubpSpec -> Params -> Params[0]");

    if (!ada_param_spec_f_ids(&tmp, &tmp))
        error("Could not get CompilationUnit -> Body -> Item ->"
	      " SubpSpec -> Params -> Params[0] -> Ids");

    if (!ada_node_child(&tmp, 0, &i))
        error("Could not get CompilationUnit -> Body -> Item ->"
	      " SubpSpec -> Params -> Params[0] -> Ids[0]");

    printf("This should be Foo:\n");
    dump_identifier(&foo);

    printf("This should be I:\n");
    dump_identifier(&i);

    if (!ada_name_p_name_matches(&foo, &foo, &boolean))
      error("Call to Foo.p_name_matches(Foo) failed");
    printf("Foo.p_name_matches(Foo) = %s\n", bool_image(boolean));

    if (!ada_name_p_name_matches(&foo, &i, &boolean))
      error ("Call to Foo.p_name_matches(I) failed");
    printf("Foo.p_name_matches(I) = %s\n", bool_image(boolean));

    ada_context_decref(ctx);
    puts("Done.");
    return 0;
}
