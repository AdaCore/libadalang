#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"

#include "langkit_text.h"
#include "utils.h"


int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;
    ada_token tok, prev_tok;

    ctx = ada_allocate_analysis_context ();
    abort_on_exception ();

    ada_initialize_analysis_context (ctx, NULL, NULL, NULL, NULL, 1, 8);
    abort_on_exception ();

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0,
                                           ada_default_grammar_rule);
    abort_on_exception ();

    prev_tok.token_data = NULL;
    prev_tok.token_index = -1;
    prev_tok.trivia_index = -1;
    for (ada_unit_first_token(unit, &tok);
         tok.token_data != NULL;
         ada_token_next(&tok, &tok))
    {
        char *kind_name = ada_token_kind_name(ada_token_get_kind(&tok));
        ada_token pt;
	ada_text text;

        ada_token_previous(&tok, &pt);
        if (!token_eq_p(&prev_tok, &pt))
            error("ada_token_previous returned an inconsistent token");

        printf("%s", kind_name);
	free (kind_name);

	ada_token_range_text (&tok, &tok, &text);
        if (text.length > 0) {
            printf(" ");
            fprint_text(stdout, text, true);
        }
	ada_destroy_text (&text);
        printf("\n");
        prev_tok = tok;
    }

    ada_context_decref(ctx);

    puts("Done.");
    return 0;
}
