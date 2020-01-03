#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"

#include "langkit_text.h"
#include "utils.h"


static void
handle_exception(const char *msg)
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
    ada_token tok, prev_tok;

    ctx = ada_create_analysis_context(NULL, NULL, 1, 8);
    if (ctx == NULL)
        handle_exception("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0,
                                           ada_default_grammar_rule);
    if (unit == NULL)
        handle_exception("Could not create the analysis unit from foo.adb");

    prev_tok.token_data = NULL;
    prev_tok.token_index = -1;
    prev_tok.trivia_index = -1;
    for (ada_unit_first_token(unit, &tok);
         tok.token_data != NULL;
         ada_token_next(&tok, &tok))
    {
        char *kind_name = ada_token_kind_name(tok.kind);
        ada_token pt;

        ada_token_previous(&tok, &pt);
        if (!token_eq_p(&prev_tok, &pt))
            error("ada_token_previous returned an inconsistent token");

        printf("%s", kind_name);
	free (kind_name);
        if (tok.text.length > 0) {
            printf(" ");
            fprint_text(stdout, tok.text, true);
        }
        printf("\n");
        prev_tok = tok;
    }

    ada_context_decref(ctx);

    puts("Done.");
    return 0;
}
