#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

#include "langkit_text.h"
#include "utils.h"


static void
run_test (int *line_mode)
{
  ada_gpr_project_scenario_variable scn_var_trail = {NULL, NULL};
  ada_string_array_ptr errors;
  ada_gpr_project gpr;
  ada_file_reader fr;
  ada_analysis_context ctx;
  ada_analysis_unit unit;
  ada_token first_token, last_token;
  ada_text text;

  printf ("== line_mode=");
  if (line_mode == NULL)
    printf ("NULL");
  else
    printf ("%i", *line_mode);
  printf(" ==\n\n");

  ada_gpr_project_load ("foo.gpr", &scn_var_trail, NULL, NULL, &gpr, &errors);
  abort_on_exception ();
  ada_free_string_array (errors);

  fr = ada_gpr_project_create_preprocessor(gpr, NULL, line_mode);
  abort_on_exception ();

  ctx = ada_create_analysis_context (
    /* charset= */ NULL,
    /* file_reader= */ fr,
    /* unit_provider= */ NULL,
    /* event_handler= */ NULL,
    /* with_trivia= */ 1,
    /* tab_stop= */ 8
  );
  abort_on_exception ();

  unit = ada_get_analysis_unit_from_file (
    /* context= */ ctx,
    /* filename= */ "foo.adb",
    /* charset= */ NULL,
    /* reparse= */ 0,
    /* rule= */ ada_default_grammar_rule
  );
  abort_on_exception ();

  ada_unit_first_token (unit, &first_token);
  abort_on_exception ();
  ada_unit_last_token (unit, &last_token);
  abort_on_exception ();
  ada_token_range_text (&first_token, &last_token, &text);
  abort_on_exception ();

  fprint_text (stdout, text, 0);
  puts ("\n");

  ada_destroy_text (&text);
  abort_on_exception ();

  ada_context_decref (ctx);
  abort_on_exception ();

  ada_dec_ref_file_reader (fr);
  abort_on_exception ();

  ada_gpr_project_free (gpr);
  abort_on_exception ();
}


int
main(void)
{
  int line_mode;
  int *line_mode_ref = &line_mode;

  line_mode = 0;
  run_test (line_mode_ref);

  line_mode = 1;
  run_test (line_mode_ref);

  line_mode = 2;
  run_test (line_mode_ref);

  run_test (NULL);

  puts("Done.");
  return 0;
}
