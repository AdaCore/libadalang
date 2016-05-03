#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

#include "langkit_dump.h"


int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;
    unsigned i;

    libadalang_initialize();
    ctx = ada_create_analysis_context("iso-8859-1");
    if (ctx == NULL)
        error("Could not create the analysis context\n");

    /* This file does not exist, so we should get diagnostics.  */
    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0, 0);

    /* Print diagnostics, converting them using text_to_locale_string.  */
    puts("Diagnostics for foo.adb");
    for (i = 0; i < ada_unit_diagnostic_count(unit); ++i) {
        ada_diagnostic d;
        char *message;

        if (!ada_unit_diagnostic(unit, i, &d))
            error("Error while getting a diagnostic");
        printf("  ");
        if (d.sloc_range.start.line != 0) {
            print_sloc_range(&d.sloc_range);
            printf(": ");
        }
        message = ada_text_to_locale_string (d.message);
        puts(message);
        free(message);
    }

    ada_destroy_analysis_context(ctx);
    puts("Done");
    return 0;
}
