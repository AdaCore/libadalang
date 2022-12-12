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
    unsigned n;
    ada_diagnostic diag;

    ctx = ada_allocate_analysis_context ();
    abort_on_exception ();

    ada_initialize_analysis_context (ctx, NULL, NULL, NULL, NULL, 1, 8);
    abort_on_exception ();

    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0,
                                           ada_default_grammar_rule);
    if (unit == NULL)
        error("Creating an analysis unit from foo.adb (a source with syntax"
              " errors) did not work");

    for (n = 0; n < ada_unit_diagnostic_count(unit); ++n)
      {
	if (!ada_unit_diagnostic(unit, n, &diag))
	  error("Could not retrieve a diagnostic");
	printf("Diagnostic: %u:%u-%u:%u: ",
	       diag.sloc_range.start.line,
	       diag.sloc_range.start.column,
	       diag.sloc_range.end.line,
	       diag.sloc_range.end.column);
	fprint_text(stdout, diag.message, false);
	putchar('\n');
      }

    ada_context_decref(ctx);
    puts("Done.");
    return 0;
}
