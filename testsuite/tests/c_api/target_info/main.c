#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_text.h"
#include "utils.h"

static void
eval_and_print (const char *label, ada_node *decl, ada_node *expr)
{
  ada_text img;
  ada_big_integer result;

  printf ("%s\n", label);

  ada_node_image (decl, &img);
  abort_on_exception ();
  fprint_text (stdout, img, false);
  ada_destroy_text (&img);
  abort_on_exception ();
  printf (" = ");

  ada_expr_p_eval_as_int (expr, &result);
  abort_on_exception ();

  ada_big_integer_text (result, &img);
  abort_on_exception ();

  ada_big_integer_decref (result);
  abort_on_exception ();

  fprint_text (stdout, img, false);
  ada_destroy_text (&img);
  abort_on_exception ();

  puts ("\n");
}

static void
check (const char *filename)
{
  ada_analysis_context ctx;
  ada_analysis_unit unit;
  ada_node n, decl, expr;
  ada_target_information info;

  printf ("== %s ==\n\n", filename);
  info = ada_target_info_load (filename);
  if (print_exception (true))
    {
      puts ("");
      return;
    }

  ctx = ada_allocate_analysis_context ();
  abort_on_exception ();

  ada_initialize_analysis_context (ctx, NULL, NULL, NULL, NULL, 1, 8);
  abort_on_exception ();

  unit = ada_get_analysis_unit_from_file (ctx, "pkg.ads", NULL, 0,
					  ada_default_grammar_rule);
  if (unit == NULL)
      error ("Could not create the analysis unit from foo.adb");

  ada_unit_root (unit, &n);
  abort_on_exception ();

  ada_compilation_unit_f_body (&n, &n);
  abort_on_exception ();

  ada_library_item_f_item (&n, &n);
  abort_on_exception ();

  ada_base_package_decl_f_public_part (&n, &n);
  abort_on_exception ();

  ada_declarative_part_f_decls (&n, &n);
  abort_on_exception ();

  ada_node_child (&n, 0, &decl);
  abort_on_exception ();

  ada_number_decl_f_expr (&decl, &expr);
  abort_on_exception ();

  eval_and_print ("Before set_target_information", &decl, &expr);

  ada_target_info_set (ctx, info);
  abort_on_exception ();
  eval_and_print ("After set_target_information", &decl, &expr);

  ada_context_decref (ctx);
  ada_target_info_free (info);
}

int
main (void)
{
  check ("no_such_file.txt");
  check ("invalid.txt");
  check ("small_ints.txt");
  puts ("Done");
  return 0;
}
