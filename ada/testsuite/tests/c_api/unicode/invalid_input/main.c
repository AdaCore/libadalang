#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

#include "langkit_dump.h"
#include "unicode_utils.h"


int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    const size_t iso_8859_1_length = strlen(src_buffer_iso_8859_1);

    libadalang_initialize();
    ctx = ada_create_analysis_context("utf-8", 0, NULL);
    if (ctx == NULL)
        error("Could not create the analysis context\n");

    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL,
                                             src_buffer_iso_8859_1,
                                             iso_8859_1_length);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    dump_diagnostics(unit, "foo.adb");

    ada_destroy_analysis_context(ctx);
    puts("Done");
    return 0;
}
