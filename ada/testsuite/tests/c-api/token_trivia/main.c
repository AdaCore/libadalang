#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"

#include "langkit_text.h"


static void
error(const char *msg)
{
    const ada_exception *exc = ada_get_last_exception();

    fputs(msg, stderr);
    if (exc != NULL) {
        puts("Last Ada exception:");
        puts(exc->information);
    }

    exit(1);
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;
    ada_token tok;

    libadalang_initialize();
    ctx = ada_create_analysis_context(NULL);
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0, 1);
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    for (ada_unit_first_token(unit, &tok);
         tok.token_data != NULL;
         ada_token_next(&tok, &tok))
    {
        char *kind_name = ada_token_kind_name(tok.kind);
        printf("%s", kind_name);
        if (tok.text.length > 0) {
            printf(" ");
            fprint_text(stdout, tok.text, true);
        }
        printf("\n");
    }

    ada_destroy_analysis_context (ctx);

    puts("Done.");
    return 0;
}
