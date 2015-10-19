#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

#include "langkit_text.h"
#include "utils.h"
#include "unicode_utils.h"


int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    const size_t iso_8859_1_length = strlen(src_buffer_iso_8859_1);

    libadalang_initialize();
    ctx = ada_create_analysis_context("utf-8");
    if (ctx == NULL)
        error("Could not create the analysis context\n");

    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL,
                                             src_buffer_iso_8859_1,
                                             iso_8859_1_length);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    /* TODO: right now, Quex calls exit() when it cannot decode the input, so
       we cannot test anything. But assuming it wasn't, how should the API
       behave?  */
    printf("Here's the string literal we parsed:\n");
    fprint_text(stdout, get_string_literal(unit), false);
    putchar('\n');

    ada_destroy_analysis_context(ctx);
    puts("Done");
    return 0;
}
