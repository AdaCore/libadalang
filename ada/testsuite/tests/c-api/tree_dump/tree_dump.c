#include <stdio.h>
#include <stdlib.h>

#include "libadalang.h"

#include "langkit_dump.h"


int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    libadalang_initialize();
    ctx = ada_create_analysis_context();
    if (ctx == NULL)
        error("Could not create the analysis context\n");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", 0);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    dump(ada_unit_root(unit), 0);

    ada_destroy_analysis_context(ctx);
    puts("Done");
    return 0;
}
