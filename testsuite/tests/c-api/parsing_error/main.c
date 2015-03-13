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
    ada_node tree;

    ctx = ada_create_analysis_context();
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_create_analysis_unit_from_file(ctx, "foo.adb");
    if (unit == NULL)
        error("Creating an analysis unit from foo.adb (a source with syntax"
              " errors) did not work");

    tree = ada_unit_root(unit);
    if (tree != NULL)
        error("Got a non-null node for a source with syntax errors");

    ada_destroy_analysis_context(ctx);
    puts("Done.");
    return 0;
}
