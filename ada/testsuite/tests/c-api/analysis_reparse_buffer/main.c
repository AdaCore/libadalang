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

void check(ada_analysis_unit unit)
{
    ada_node prelude_list, with_decl;
    int is_limited;

    if (unit == NULL)
        error("Could not create the analysis unit for foo.adb from a buffer");

    if (!ada_compilation_unit_f_prelude(ada_unit_root(unit), &prelude_list)
        || !ada_node_child(prelude_list, 0, &with_decl)
        || !ada_with_decl_f_is_limited(with_decl, &is_limited))
        error("Could not traverse the AST as expected");
    printf("WithDecl: is_limited = %s\n", is_limited ? "true" : "false");
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    const size_t src_buffer_1_length = strlen(src_buffer_1);
    const size_t src_buffer_2_length = strlen(src_buffer_2);

    libadalang_initialize();
    ctx = ada_create_analysis_context("iso-8859-1");
    if (ctx == NULL)
        error("Could not create the analysis context");

    /* Make sure the first parsing (with the "limited" keyword) works properly
       and check is_limited.  */
    puts("1. Parsing using buffer 1");
    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL,
                                             src_buffer_1,
                                             src_buffer_1_length);
    check(unit);

    /* Now make sure getting the unit with reparsing (without the "limited"
       keyword) clears is_limited.  */
    puts("2. Reparsing from context using buffer 2");
    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL,
                                             src_buffer_2,
                                             src_buffer_2_length);
    check(unit);

    /* Finally make sure reparsing the unit (with the "limited" keyword) sets
       is_limited.  */
    puts("3. Reparsing from unit using buffer 1");
    ada_unit_reparse_from_buffer(unit, NULL,
                                 src_buffer_1, src_buffer_1_length);
    check(unit);

    ada_destroy_analysis_context(ctx);
    puts("Done.");
    return 0;
}
