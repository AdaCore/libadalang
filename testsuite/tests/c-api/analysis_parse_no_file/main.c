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

    ctx = ada_create_analysis_context();
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", 0);
    if (unit != NULL)
        error("Creating an analysis unit for foo.adb worked whereas there is"
              " no such file");

    ada_destroy_analysis_context(ctx);
    puts("Done.");
    return 0;
}
