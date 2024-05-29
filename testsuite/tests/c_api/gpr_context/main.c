#include <stdio.h>
#include <stdlib.h>

#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_text.h"

static uint32_t pkg_buffer[3] = {'p', 'k', 'g'};
static ada_text pkg_text = {pkg_buffer, 3, 0};


static void
check (const char *label,
       const char *root_project,
       const char *project,
       ada_event_handler event_handler,
       int with_trivia,
       int tab_stop)
{
  ada_gpr_project_scenario_variable scn_var_trail = {NULL, NULL};
  ada_gpr_project gpr;
  ada_string_array_ptr errors;
  ada_analysis_context ctx;
  const ada_exception *exc;
  ada_analysis_unit u;
  ada_token t;
  ada_token_kind k;
  ada_text text;
  ada_node n;
  ada_node_array nodes;
  int count, i;
  char *name;

  printf("== %s ==\n\n", label);

  /* Load the requested project tree.  */
  ada_gpr_project_load (root_project, &scn_var_trail, NULL, NULL, NULL, &gpr,
			&errors);
  abort_on_exception ();

  ada_free_string_array (errors);
  abort_on_exception ();

  /* Create the analysis context from that project. This may fail if the
     project is an aggregate, so protect against specific exceptions.  */
  ctx = ada_allocate_analysis_context ();
  abort_on_exception ();

  ada_gpr_project_initialize_context (gpr, ctx, project, event_handler,
				      with_trivia, tab_stop);
  exc = ada_get_last_exception ();
  if (exc != NULL)
    {
      if (exc->kind != EXCEPTION_UNSUPPORTED_VIEW_ERROR)
	{
	  abort_on_exception ();
	}
      printf ("Unsupported_View_Error: %s\n\n", exc->information);

      ada_context_decref (ctx);
      ada_gpr_project_free (gpr);
      return;
    }

  u = ada_get_analysis_unit_from_provider (ctx, &pkg_text,
					   ADA_ANALYSIS_UNIT_KIND_UNIT_BODY,
					   NULL, 0);
  abort_on_exception ();

  count = ada_unit_diagnostic_count (u);
  abort_on_exception ();

  if (count > 0)
    {
      dump_diagnostics (u, "pkg");
      exit (1);
    }

  /* To show that With_Trivia / Tab_Stop are properly forwarded to the analysis
     context constructor and that the default charset is correctly determined,
     show the first token (or trivia).  */
  ada_unit_first_token (u, &t);
  abort_on_exception ();

  k = ada_token_get_kind (&t);
  abort_on_exception ();

  name = ada_token_kind_name (k);
  abort_on_exception ();

  printf ("pkg%%b first token/trivia: <Token Kind=%s Text=", name);
  free (name);

  ada_token_range_text (&t, &t, &text);
  abort_on_exception ();

  fprint_text (stdout, text, true);
  puts (">");
  ada_destroy_text (&text);
  abort_on_exception ();

  ada_unit_root (u, &n);
  abort_on_exception ();

  ada_node_image (&n, &text);
  abort_on_exception ();

  printf ("pkg%%b root node: ");
  fprint_text (stdout, text, false);
  puts ("");

  ada_destroy_text (&text);
  abort_on_exception ();

  /* To show that the unit provider works as expected, resolve the Pkg package
     spec from its body.  */

  ada_unit_root (u, &n);
  abort_on_exception ();

  ada_compilation_unit_f_body (&n, &n);
  abort_on_exception ();

  ada_library_item_f_item (&n, &n);
  abort_on_exception ();

  ada_body_node_p_previous_part (&n, 0, &n);
  abort_on_exception ();

  ada_node_image (&n, &text);
  abort_on_exception ();

  printf ("pkg%%b previous part: ");
  fprint_text (stdout, text, false);
  puts ("");

  ada_destroy_text (&text);
  abort_on_exception ();

  ada_unit_root (u, &n);
  abort_on_exception ();

  /* To show that configuration pragmas are properly detected from the project,
     print their list.  */
  ada_compilation_unit_p_all_config_pragmas (&n, &nodes);
  abort_on_exception ();

  for (i = 0; i < nodes->n; ++i)
    {
      ada_node_image (&nodes->items[i], &text);
      abort_on_exception ();

      printf ("Config pragma: ");
      fprint_text (stdout, text, false);
      puts ("");

      ada_destroy_text (&text);
      abort_on_exception ();
    }

  ada_node_array_dec_ref (nodes);
  abort_on_exception ();

  ada_context_decref (ctx);
  ada_gpr_project_free (gpr);

  puts ("");
}

static void
eh_destroy (void *data)
{
  (void) data;
}

static void
eh_unit_requested (void *data, ada_analysis_context ctx, ada_text *name,
		   ada_analysis_unit from, ada_bool found,
		   ada_bool is_not_found_error)
{
  (void) data;
  (void) ctx;
  (void) name;
  (void) from;
  (void) found;
  (void) is_not_found_error;
}

static int eh_triggered = 0;

static void
eh_unit_parsed (void *data, ada_analysis_context ctx, ada_analysis_unit unit,
		ada_bool reparsed)
{
  (void) data;
  (void) ctx;
  (void) unit;
  (void) reparsed;
  if (! eh_triggered)
    {
      eh_triggered = 1;
      puts ("Unit_Parsed_Callback invoked");
    }
}

int
main(void)
{
  ada_event_handler event_handler;

  check (/* label */ "Simple: defaults",
	 /* root_project */ "simple/p.gpr",
	 /* project */ NULL,
	 /* event_handler */ NULL,
	 /* with_trivia */ 1,
	 /* tab_stop */ 8);
  check (/* label */ "Simple: without trivia",
	 /* root_project */ "simple/p.gpr",
	 /* project */ NULL,
	 /* event_handler */ NULL,
	 /* with_trivia */ 0,
	 /* tab_stop */ 8);
  check (/* label */ "Simple: tab stop = 4",
	 /* root_project */ "simple/p.gpr",
	 /* project */ NULL,
	 /* event_handler */ NULL,
	 /* with_trivia */ 1,
	 /* tab_stop */ 4);

  check (/* label */ "UTF-8",
	 /* root_project */ "utf-8/p.gpr",
	 /* project */ NULL,
	 /* event_handler */ NULL,
	 /* with_trivia */ 1,
	 /* tab_stop */ 8);

  check (/* label */ "Aggregate project (no specific view)",
	 /* root_project */ "aggregate/agg.gpr",
	 /* project */ NULL,
	 /* event_handler */ NULL,
	 /* with_trivia */ 1,
	 /* tab_stop */ 8);
  check (/* label */ "Aggregate project (specific view: p2)",
	 /* root_project */ "aggregate/agg.gpr",
	 /* project */ "p2",
	 /* event_handler */ NULL,
	 /* with_trivia */ 1,
	 /* tab_stop */ 8);

  event_handler = ada_create_event_handler (NULL, eh_destroy,
					    eh_unit_requested, eh_unit_parsed);
  abort_on_exception ();
  check (/* label */ "Simple: event handler",
	 /* root_project */ "simple/p.gpr",
	 /* project */ NULL,
	 /* event_handler */ event_handler,
	 /* with_trivia */ 1,
	 /* tab_stop */ 8);
  ada_dec_ref_event_handler (event_handler);
  abort_on_exception ();

  check (/* label */ "Preprocessing (p1)",
	 /* root_project */ "preprocessing/p1.gpr",
	 /* project */ NULL,
	 /* event_handler */ NULL,
	 /* with_trivia */ 1,
	 /* tab_stop */ 8);
  check (/* label */ "Preprocessing (p2)",
	 /* root_project */ "preprocessing/p2.gpr",
	 /* project */ NULL,
	 /* event_handler */ NULL,
	 /* with_trivia */ 1,
	 /* tab_stop */ 8);

  check (/* label */ "Config pragmas (p1)",
	 /* root_project */ "config_pragmas/p1.gpr",
	 /* project */ NULL,
	 /* event_handler */ NULL,
	 /* with_trivia */ 1,
	 /* tab_stop */ 8);
  check (/* label */ "Config pragmas (p2)",
	 /* root_project */ "config_pragmas/p2.gpr",
	 /* project */ NULL,
	 /* event_handler */ NULL,
	 /* with_trivia */ 1,
	 /* tab_stop */ 8);

  puts("Done.");
  return 0;
}
