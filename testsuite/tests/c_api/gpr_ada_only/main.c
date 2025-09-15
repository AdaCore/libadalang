#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

#include "langkit_text.h"
#include "utils.h"


static void
check (int ada_only)
{
  ada_gpr_options opts;
  ada_string_array_ptr errors;
  ada_gpr_project gpr;
  int i;

  if (ada_only)
    puts ("== Ada only ==\n");
  else
    puts ("== All languages ==\n");

  opts = ada_gpr_options_create ();
  abort_on_exception ();

  ada_gpr_options_add_switch (opts, ADA_GPR_OPTION_P, "foo.gpr", NULL, 0);
  abort_on_exception ();

  ada_gpr_project_load (opts, ada_only, &gpr, &errors);
  print_exception (false);

  for (i = 0; i < errors->length; ++i)
    printf ("error: %s\n", errors->c_ptr[i]);
  puts ("");

  ada_free_string_array (errors);
  abort_on_exception ();

  ada_gpr_project_free (gpr);
  abort_on_exception ();

  ada_gpr_options_free (opts);
  abort_on_exception ();
}


int
main (void)
{
  check (0);
  check (1);
  puts("Done.");
  return 0;
}
