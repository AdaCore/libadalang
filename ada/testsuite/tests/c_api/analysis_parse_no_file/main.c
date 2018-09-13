#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"

#include "langkit_dump.h"


int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;
    ada_base_entity root;

    libadalang_initialize();
    ctx = ada_create_analysis_context(NULL, NULL, 1, 8);
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0);
    if (unit == NULL)
        error("Could not create the analysis unit for foo.adb");

    ada_unit_root(unit, &root);
    if (!ada_node_is_null(&root))
        error("Got an AST for foo.adb whereas the file does not exists");

    dump_diagnostics(unit, "foo.adb");

    ada_context_decref(ctx);
    puts("Done.");
    return 0;
}
