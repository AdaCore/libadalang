#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libadalang.h"


const char *src_buffer = (
  "limited with Ada.Text_IO;\n"
  "\n"
  "procedure Foo is\n"
  "   function \"+\" (S : String) return String is (S);\n"
  "begin\n"
  "   Ada.Text_IO.Put_Line (+\"Hello, world!\");\n"
  "end Foo;\n"
);


static void
error(const char *msg)
{
    fputs(msg, stderr);
    exit(1);
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    ctx = ada_create_analysis_context(NULL, NULL, NULL, 1, 8);
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL,
                                             src_buffer, strlen(src_buffer),
                                             ada_default_grammar_rule);
    if (unit == NULL)
        error("Could not create the analysis unit for foo.adb from a buffer");

    ada_context_decref(ctx);
    puts("Done.");
    return 0;
}
