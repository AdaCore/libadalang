#include <stdio.h>
#include <stdlib.h>

#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_text.h"

int
main (void)
{
    ada_gpr_project gpr;
    ada_string_array_ptr errors;
    ada_gpr_project_scenario_variable scn_var_trail = {NULL, NULL};

    ada_gpr_project_load ("simple/p.gpr", &scn_var_trail, NULL, NULL,
			  "empty.cgpr", 0, &gpr, &errors);
    if (print_exception (false))
      return 0;

    /* This is supposed to be dead code (the project loading is supposed to
       abort with a fatal error), but just in case it does not, print the
       errors we get.  */
    if (errors->length > 0)
      {
        printf ("Errors:\n");
        for (int i = 0 ; i < errors->length; i++)
	  {
	    printf ("   - %s\n", errors->c_ptr[i]);
	  }
      }

    ada_gpr_project_free (gpr);
    abort_on_exception ();
    ada_free_string_array (errors);
    abort_on_exception ();
    return 0;
}
