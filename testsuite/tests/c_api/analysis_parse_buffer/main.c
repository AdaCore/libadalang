#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libadalang.h"

#include "utils.h"


const char *src_buffer = (
  "limited with Ada.Text_IO;\n"
  "\n"
  "procedure Foo is\n"
  "   function \"+\" (S : String) return String is (S);\n"
  "begin\n"
  "   Ada.Text_IO.Put_Line (+\"Hello, world!\");\n"
  "end Foo;\n"
);

int
main(void)
{
    ada_analysis_context ctx;

    ctx = ada_allocate_analysis_context ();
    abort_on_exception ();

    ada_initialize_analysis_context (ctx, NULL, NULL, NULL, NULL, 1, 8);
    abort_on_exception ();

    ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL, src_buffer,
				      strlen(src_buffer),
				      ada_default_grammar_rule);
    abort_on_exception ();

    ada_context_decref(ctx);
    abort_on_exception ();

    puts("Done.");
    return 0;
}
