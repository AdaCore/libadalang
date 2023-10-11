#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_text.h"
#include "utils.h"


const char *sources[2] = {"foo.adb", "bar.adb"};


static ada_analysis_unit
load (ada_analysis_context ctx, const char *filename)
{
  int count;
  ada_analysis_unit u
    = ada_get_analysis_unit_from_file (ctx, filename, NULL, 0,
				       ada_default_grammar_rule);
  abort_on_exception ();

  count = ada_unit_diagnostic_count (u);
  abort_on_exception ();

  if (count > 0)
    {
      dump_diagnostics (u, filename);
      exit (1);
    }

  return u;
}


static void
check_pragmas (ada_analysis_context ctx)
{
  int i, j;
  ada_analysis_unit u;
  ada_node n;
  ada_node_array nodes;
  ada_text text;

  for (i = 0; i < 2; ++i)
    {
      const char *filename = sources[i];

      printf ("Pragmas for %s:\n", filename);

      u = load (ctx, filename);

      ada_unit_root (u, &n);
      abort_on_exception ();

      ada_compilation_unit_p_all_config_pragmas (&n, &nodes);
      if (print_exception (true))
	continue;

      for (j = 0; j < nodes->n; ++j)
	{
	  ada_node_image (&nodes->items[j], &text);
	  abort_on_exception ();

	  printf ("Config pragma: ");
	  fprint_text (stdout, text, false);
	  puts ("");

	  ada_destroy_text (&text);
	  abort_on_exception ();
	}

      ada_node_array_dec_ref (nodes);
      abort_on_exception ();
    }
}


int
main (void)
{
  ada_analysis_context ctx;

  ada_analysis_unit global;
  ada_analysis_unit ua1[1];
  ada_analysis_unit ua3[3];
  ada_analysis_unit ua5[5];
  ada_analysis_unit ua_double[5];

  /* Create a single context for the whole test.  */
  ctx = ada_allocate_analysis_context ();
  abort_on_exception ();

  ada_initialize_analysis_context (ctx, NULL, NULL, NULL, NULL, 1, 8);
  abort_on_exception ();

  /* Fetch config pragma files.  */
  global = load (ctx, "global.adc");

  ua1[0] = NULL;

  ua3[0] = load (ctx, "foo.adb");
  ua3[1] = load (ctx, "local_1.adc");
  ua3[2] = NULL;

  ua5[0] = load (ctx, "foo.adb");
  ua5[1] = load (ctx, "local_1.adc");
  ua5[2] = load (ctx, "bar.adb");
  ua5[3] = load (ctx, "local_2.adc");
  ua5[4] = NULL;

  ua_double[0] = ua5[0];
  ua_double[1] = ua5[1];
  ua_double[2] = ua5[0];
  ua_double[3] = ua5[3];
  ua_double[4] = ua5[4];

  puts ("== Query config pragmas with no mappings set ==");
  check_pragmas (ctx);
  puts ("");

  puts ("== Set empty mappings ==");
  ada_set_config_pragmas_mapping (ctx, NULL, ua1);
  abort_on_exception ();
  check_pragmas (ctx);
  puts ("");

  puts ("== Set global-only mapping ==");
  ada_set_config_pragmas_mapping (ctx, global, ua1);
  abort_on_exception ();
  check_pragmas (ctx);
  puts ("");

  puts ("== Set local mapping for foo.adb ==");
  ada_set_config_pragmas_mapping (ctx, global, ua3);
  abort_on_exception ();
  check_pragmas (ctx);
  puts ("");

  puts ("== Set all mappings ==");
  ada_set_config_pragmas_mapping (ctx, global, ua5);
  abort_on_exception ();
  check_pragmas (ctx);
  puts ("");

  puts ("== Double unit key (error) ==");
  ada_set_config_pragmas_mapping (ctx, global, ua_double);
  print_exception (false);
  check_pragmas (ctx);
  puts ("");

  ada_context_decref (ctx);

  puts ("Done.");
  return 0;
}
