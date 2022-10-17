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
  bool had_exception;

  printf ("== %s ==\n", project_file);

  ada_gpr_project_load (project_file, &scn_var_trail, NULL, NULL, &gpr,
			&errors);
  had_exception = print_exception (false);

  if (had_exception)
    puts ("");
  else
    {
      for (i = 0; i < errors->length; ++i)
	printf ("error: %s\n", errors->c_ptr[i]);
      puts ("");

      ada_free_string_array (errors);
      abort_on_exception ();

      ada_gpr_project_free (gpr);
      abort_on_exception ();
    }
}


int
main (void)
{
  run("nosuchgpr.gpr");
  run("nosuchtarget.gpr");
  run("missingdep.gpr");
  puts("Done.");
  return 0;
}
