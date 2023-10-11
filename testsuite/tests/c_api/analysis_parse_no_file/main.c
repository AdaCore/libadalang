#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"

#include "langkit_dump.h"
#include "utils.h"


int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;
    ada_node root;

    ctx = ada_allocate_analysis_context ();
    abort_on_exception ();

    ada_initialize_analysis_context (ctx, NULL, NULL, NULL, NULL, 1, 8);
    abort_on_exception ();

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0,
					   ada_default_grammar_rule);
    abort_on_exception ();

    ada_unit_root(unit, &root);
    abort_on_exception ();

    dump_diagnostics(unit, "foo.adb");

    ada_context_decref(ctx);
    abort_on_exception ();

    puts("Done.");
    return 0;
}
