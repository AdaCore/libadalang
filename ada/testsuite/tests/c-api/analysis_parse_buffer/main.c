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

    libadalang_initialize();
    ctx = ada_create_analysis_context("iso-8859-1");
    if (ctx == NULL)
        error("Could not create the analysis context");

    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL,
                                             src_buffer, strlen(src_buffer),
                                             0);
    if (unit == NULL)
        error("Could not create the analysis unit for foo.adb from a buffer");

    ada_destroy_analysis_context(ctx);
    puts("Done.");
    return 0;
}
