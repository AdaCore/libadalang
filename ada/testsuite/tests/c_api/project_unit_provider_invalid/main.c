#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_text.h"

static uint32_t filename_1[] = {233};
static uint32_t filename_2[] = {'u', 'n', 'k', 'n', 'o', 'w', 'n', '_',
                                'u', 'n', 'i', 't'};

static uint32_t *filenames[] = {filename_1, filename_2};
static size_t filename_sizes[] = {1, 12};

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;
    int i;

    libadalang_initialize();
    ctx = ada_create_analysis_context(NULL, NULL, 1, 8);
    if (ctx == NULL)
        error("Could not create the analysis context");

    for (i = 0; i < 2; ++i) {
        ada_text unit_name = { filenames[i], filename_sizes[i], true };

        printf("Trying to get unit: ");
        fprint_text(stdout, unit_name, true);
        printf("\n");

        unit = ada_get_analysis_unit_from_provider(
            ctx, &unit_name, ADA_ANALYSIS_UNIT_KIND_UNIT_SPECIFICATION, NULL, 0
        );
        if (unit == NULL)
            printf("   ... got a null unit\n");
        else {
            printf("   ... got an analysis unit:\n");
            dump_diagnostics(unit, "???");
        }
    }

    ada_context_decref(ctx);
    puts("Done.");
    return 0;
}
