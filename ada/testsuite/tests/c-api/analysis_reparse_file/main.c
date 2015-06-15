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

void write_source(const char *src_buffer)
{
    FILE *f;

    f = fopen("foo.adb", "w");
    fputs(src_buffer, f);
    fclose(f);
}

void check(ada_analysis_unit unit)
{
    ada_node prelude_list, with_decl;
    int is_limited;

    if (unit == NULL)
        error("Could not create the analysis unit for foo.adb from a file");

    if (!ada_compilation_unit_prelude(ada_unit_root(unit), &prelude_list)
        || !ada_node_child(prelude_list, 0, &with_decl)
        || !ada_with_decl_is_limited(with_decl, &is_limited))
        error("Could not traverse the AST as expected");
    printf("WithDecl: is_limited = %s\n", is_limited ? "true" : "false");
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    libadalang_initialize();
    ctx = ada_create_analysis_context();
    if (ctx == NULL)
        error("Could not create the analysis context");

    /* First work with the "limited" keyword.  */
    write_source(src_buffer_1);

    puts("1. Parsing source 1");
    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", 0);
    check(unit);

    /* Now work without the "limited" keyword:
        2. getting the unit without reparsing should preserve is_limited;
        3. trying to reparse the deleted file should raise an error but
           preserve the unit's tree;
        4. getting the unit with reparsing should clear is_limited.  */
    write_source(src_buffer_2);

    puts("2. Parsing source 2 (reparse=false)");
    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", 0);
    check(unit);

    remove("foo.adb");

    puts("3. Parsing with deleted file (reparse=true)");
    if (ada_get_analysis_unit_from_file(ctx, "foo.adb", 1) != NULL)
        error("Reparsing analysis unit from a deleted file returned non-null");
    check(unit);

    write_source(src_buffer_2);

    puts("4. Parsing source 2 (reparse=true)");
    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", 1);
    check(unit);

    write_source(src_buffer_1);

    /* Now restore the "limited" keyword in the soruce:
        5. reparsing the unit should work and set is_limited;
        6. reparsing the unit with a deleted file should raise an error but
           preserve the unit's tree. */

    puts("5. Reparsing source 1");
    if (!ada_unit_reparse_from_file(unit))
        error("Could not reparse foo.adb");
    check(unit);

    remove("foo.adb");

    puts("6. Reparsing with deleted file");
    if (ada_unit_reparse_from_file(unit))
        error("Reparsing foo.adb was supposed to fail");
    check(unit);

    ada_destroy_analysis_context(ctx);
    puts("Done.");
    return 0;
}
