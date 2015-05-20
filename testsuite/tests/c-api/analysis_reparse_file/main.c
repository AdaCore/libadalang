#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libadalang.h"


const char *src_buffer_1 = (
  "limited with Ada.Text_IO;\n"
  "\n"
  "procedure Foo is\n"
  "   function \"+\" (S : String) return String is (S);\n"
  "begin\n"
  "   Ada.Text_IO.Put_Line (+\"Hello, world!\");\n"
  "end Foo;\n"
);

const char *src_buffer_2 = (
  "with Ada.Text_IO;\n"
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
get_unit(ada_analysis_context ctx,
         const char *src_buffer,
         int reparse)
{
    FILE *f;
    ada_analysis_unit unit;
    ada_node prelude_list, with_decl;
    int is_limited;

    /* Create a source file from "src_buffer".  */
    f = fopen("foo.adb", "w");
    fputs(src_buffer, f);
    fclose(f);

    /* Then (re)parse as required.  */
    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", reparse);
    if (unit == NULL)
        error("Could not create the analysis unit for foo.adb from a"
              " file");

    /* And finally see if the WithDecl node has the is_limited bit set.  */
    if (!ada_compilation_unit_prelude(ada_unit_root(unit), &prelude_list)
        || !ada_node_child(prelude_list, 0, &with_decl)
        || !ada_with_decl_is_limited(with_decl, &is_limited))
        error("Could not traverse the AST as expected");
    return is_limited;
}

int
main(void)
{
    ada_analysis_context ctx;
    int is_limited;

    ctx = ada_create_analysis_context();
    if (ctx == NULL)
        error("Could not create the analysis context");

    is_limited = get_unit(ctx, src_buffer_1, 0);
    printf("First get_unit: is_limited = %s\n", is_limited ? "true" : "false");

    is_limited = get_unit(ctx, src_buffer_2, 0);
    printf("Second get_unit (no reparse): is_limited = %s\n",
           is_limited ? "true" : "false");

    is_limited = get_unit(ctx, src_buffer_2, 1);
    printf("Third get_unit (reparse): is_limited = %s\n",
           is_limited ? "true" : "false");

    ada_destroy_analysis_context(ctx);
    puts("Done.");
    return 0;
}
