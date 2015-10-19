#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

#include "langkit_text.h"
#include "utils.h"
#include "unicode_utils.h"


static void
check(ada_analysis_unit unit)
{
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    printf("  Got: ");
    fprint_text(stdout, get_string_literal(unit), false);
    putchar('\n');
}


int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    const size_t iso_8859_1_length = strlen(src_buffer_iso_8859_1);
    const size_t utf_8_length = strlen(src_buffer_utf_8);

    libadalang_initialize();
    ctx = ada_create_analysis_context("iso-8859-1");
    if (ctx == NULL)
        error("Could not create the analysis context\n");

    /* Check that at unit creation, we use the context-specific default
       charset.  */
    puts("1. Parsing buffer (a) with context default charset");
    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL,
                                             src_buffer_iso_8859_1,
                                             iso_8859_1_length);
    check(unit);

    puts("2. Reparsing buffer (b) with another charset");
    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", "utf-8",
                                             src_buffer_utf_8, utf_8_length);
    check(unit);

    /* Check that for reparsing, unit-specific charset takes precedence over
       the context-specific one.  */
    puts("3. Reparsing buffer (b) with the unit default charset");
    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL,
                                             src_buffer_utf_8, utf_8_length);
    check(unit);

    puts("4. Reparsing buffer (a) with the original charset");
    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", "iso-8859-1",
                                             src_buffer_iso_8859_1,
                                             iso_8859_1_length);
    check(unit);

    ada_destroy_analysis_context(ctx);
    puts("Done");
    return 0;
}
