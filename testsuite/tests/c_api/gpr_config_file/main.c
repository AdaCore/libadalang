#include <stdio.h>
#include <stdlib.h>

#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_text.h"

int
main (void)
{
    ada_gpr_options opts;
    ada_gpr_project gpr;
    ada_string_array_ptr errors;
    int had_error;

    opts = ada_gpr_options_create ();
    abort_on_exception ();
    ada_gpr_options_add_switch (opts, ADA_GPR_OPTION_P, "simple/p.gpr", NULL,
				0);
    abort_on_exception ();
    ada_gpr_options_add_switch (opts, ADA_GPR_OPTION_CONFIG, "empty.cgpr",
				NULL, 0);
    abort_on_exception ();

    ada_gpr_project_load (opts, 0, &gpr, &errors);
    had_error = print_exception (false);
    if (! had_error)
      {
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
      }

    ada_gpr_options_free (opts);
    abort_on_exception ();
    return 0;
}
