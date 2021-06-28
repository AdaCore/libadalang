#include <stdio.h>
#include <stdlib.h>

#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_text.h"

static void
println_token(ada_token *token)
{
    printf("  * ");
    print_token(token);
    printf("\n");
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit, unit2;
    ada_token first, last, empty_first, empty_last, other_unit;
    ada_text text;

    ctx = ada_create_analysis_context(NULL, NULL, NULL, NULL, 0, 8);
    if (ctx == NULL)
        error("Could not create the analysis context\n");

    unit = ada_get_analysis_unit_from_file(ctx, "test.adb", NULL, 0,
                                           ada_default_grammar_rule);
    if (unit == NULL)
        error("Could not create the analysis unit from test.adb");
    unit2 = ada_get_analysis_unit_from_file(ctx, "test2.adb", NULL, 0,
                                           ada_default_grammar_rule);
    if (unit2 == NULL)
        error("Could not create the analysis unit from test2.adb");

    ada_unit_first_token(unit, &first);
    ada_unit_last_token(unit, &last);

    ada_unit_first_token(unit, &empty_last);
    ada_unit_last_token(unit, &empty_first);
    ada_token_previous(&empty_first, &empty_first);
    ada_token_next(&empty_last, &empty_last);

    ada_unit_first_token(unit2, &other_unit);

    printf("First and last tokens for test.adb:\n");
    println_token(&first);
    println_token(&last);
    printf("\n");

    printf("Whole source buffer for test.adb:\n");
    if (!ada_token_range_text(&first, &last, &text))
        error("Could not get slice #1");
    fprint_text(stdout, text, false);
    printf("\n\n");

    printf("Empty range for the following bounds:\n");
    println_token(&empty_first);
    println_token(&empty_last);
    if (!ada_token_range_text(&empty_first, &empty_last, &text))
        error("Could not get slice #2");
    fprint_text(stdout, text, true);
    printf("\n\n");

    printf("Trying to get a source slice for two nodes in different units...");
    printf("\n");
    if (ada_token_range_text(&first, &other_unit, &text))
        printf("... got no error: unacceptable!\n");
    else
        printf("... got the expected error\n");
    printf("\n");

    ada_context_decref(ctx);
    puts("Done.");
    return 0;
}
