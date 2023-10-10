#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_find.h"
#include "langkit_text.h"

#define UNIT_SPEC ADA_ANALYSIS_UNIT_KIND_UNIT_SPECIFICATION
#define UNIT_BODY ADA_ANALYSIS_UNIT_KIND_UNIT_BODY

struct unit_ref {
  const char *name;
  ada_analysis_unit_kind kind;
};

const struct unit_ref all_units[] = {
  {"foo", UNIT_BODY},
  {"foo", UNIT_SPEC},
  {"bar", UNIT_BODY},
  {NULL, 0},
};

const struct unit_ref foo_unit[] = {
  {"foo", UNIT_BODY},
  {NULL, 0},
};

const char *all_inputs[] = {"foo-utf8.ada", "foo-utf16.ada", NULL};
const char *single_input[] = {"foo-utf8.ada", NULL};

static void
check (const char *label, const struct unit_ref *refs,
       const char **input_files, const char *charset)
{
  ada_unit_provider up;
  ada_analysis_context ctx;

  printf ("== %s ==\n", label);
  puts ("");

  up = ada_create_auto_provider (input_files, charset);
  abort_on_exception ();

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

  for (int i = 0; refs[i].name != NULL; ++i)
    {
      const ada_analysis_unit_kind kind = refs[i].kind;
      unsigned name_length = strlen (refs[i].name);
      uint32_t name[name_length];
      ada_text unit_name = {name, name_length, true};
      unsigned count;
      ada_analysis_unit unit;

      for (unsigned j = 0; j < name_length; ++j)
	name[j] = refs[i].name[j];

      unit = ada_get_analysis_unit_from_provider (
	/* context= */ ctx,
	/* name= */ &unit_name,
	/* kind= */ kind,
	/* charset= */ NULL,
	/* reparse= */ 0
      );
      abort_on_exception ();

      count = ada_unit_diagnostic_count (unit);
      abort_on_exception ();

      if (count)
	dump_diagnostics (unit, refs[i].name);
      else
	{
	  const char *kind_name = (kind == UNIT_BODY) ? "body" : "spec";
	  ada_node root;
	  ada_text image;

	  ada_unit_root (unit, &root);
	  abort_on_exception ();

	  ada_node_image (&root, &image);
	  abort_on_exception ();

	  printf ("%s/%s: ", refs[i].name, kind_name);
	  fprint_text (stdout, image, false);
	  puts ("");

	  ada_destroy_text(&image);
	  abort_on_exception ();
	}
    }
  puts ("");

  ada_context_decref (ctx);
  abort_on_exception ();

  ada_dec_ref_unit_provider (up);
  abort_on_exception ();
}

int
main(void)
{
  check ("default charset", &all_units[0], &single_input[0], NULL);
  check ("utf-8", &foo_unit[0], &all_inputs[0], "utf-8");
  check ("utf-16", &foo_unit[0], &all_inputs[0], "utf-16");
  puts("Done.");
  return 0;
}
