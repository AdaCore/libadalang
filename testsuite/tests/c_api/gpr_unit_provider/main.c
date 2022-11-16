/* Check that the gpr_project_create_unit_provider function works as
   expected.  */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

#include "langkit_text.h"
#include "utils.h"


static void
run (const char *project_file, const char *subproject)
{
  ada_gpr_project_scenario_variable scn_var_trail = {NULL, NULL};
  ada_string_array_ptr errors;
  ada_gpr_project gpr;
  ada_unit_provider up;
  int i;
  bool had_exception;

  printf ("== %s - %s ==\n\n",
	  project_file,
	  subproject == NULL ? "<none>" : subproject);

  ada_gpr_project_load (project_file, &scn_var_trail, NULL, NULL, &gpr,
			&errors);
  abort_on_exception ();

  for (i = 0; i < errors->length; ++i)
    printf ("error: %s\n", errors->c_ptr[i]);
  ada_free_string_array (errors);
  abort_on_exception ();

  up = ada_gpr_project_create_unit_provider (gpr, subproject);
  if (print_exception (true))
    puts ("");
  else
    {
      uint32_t unit_name_chars[3] = {'p', 'k', 'g'};
      ada_text unit_name = { unit_name_chars, 3, true };
      ada_analysis_unit unit;
      ada_analysis_context ctx;
      ada_token tok_first, tok_last;
      ada_text unit_text;

      ctx = ada_allocate_analysis_context ();
      abort_on_exception ();

      ada_initialize_analysis_context (
        /* context= */ ctx,
        /* charset= */ NULL,
        /* file_reader= */ NULL,
        /* unit_provider= */ up,
        /* event_handler= */ NULL,
        /* with_trivia= */ 1,
        /* tab_stop= */ 8
      );
      abort_on_exception ();

      unit = ada_get_analysis_unit_from_provider (
	/* context= */ ctx,
	/* name= */ &unit_name,
	/* kind= */ ADA_ANALYSIS_UNIT_KIND_UNIT_SPECIFICATION,
	/* charset= */ NULL,
	/* reparse= */ 0
      );
      abort_on_exception ();

      ada_unit_first_token (unit, &tok_first);
      abort_on_exception ();
      if (tok_first.context == NULL)
	puts ("No source buffer\n");
      else
	{
	  ada_unit_last_token (unit, &tok_last);
	  abort_on_exception ();

	  ada_token_range_text (&tok_first, &tok_last, &unit_text);
	  abort_on_exception ();

	  puts ("Source buffer for pkg(spec):");
	  fprint_text (stdout, unit_text, false);
	  puts ("\n");

	  ada_destroy_text (&unit_text);
	}

      ada_context_decref (ctx);
    }

  ada_dec_ref_unit_provider (up);
  ada_gpr_project_free (gpr);
  abort_on_exception ();
}


int
main (void)
{
  run("agg.gpr", NULL);
  run("agg.gpr", "p1");
  run("agg.gpr", "p2");
  run("agg.gpr", "nosuchproject");
  run("p1.gpr", NULL);
  puts("Done.");
  return 0;
}
