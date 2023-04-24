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
run (const char *project_file)
{
  ada_gpr_project_scenario_variable scn_var_trail = {NULL, NULL};
  ada_string_array_ptr errors;
  ada_gpr_project gpr;
  int i;
  char *charset;

  ada_gpr_project_load (project_file, &scn_var_trail, NULL, NULL, &gpr,
			&errors);
  abort_on_exception ();

  for (i = 0; i < errors->length; ++i)
    printf ("error: %s\n", errors->c_ptr[i]);
  ada_free_string_array (errors);
  abort_on_exception ();

  charset = ada_gpr_project_default_charset (gpr, NULL);
  abort_on_exception ();

  printf ("%s: %s\n", project_file, charset);
  free (charset);

  ada_gpr_project_free (gpr);
  abort_on_exception ();
}


int
main (void)
{
  run("default.gpr");
  run("utf8.gpr");
  puts("Done.");
  return 0;
}
