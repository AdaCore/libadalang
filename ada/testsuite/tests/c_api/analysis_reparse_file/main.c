#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libadalang.h"

#include "langkit_dump.h"


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

void write_source(const char *src_buffer)
{
    FILE *f;

    f = fopen("foo.adb", "w");
    fputs(src_buffer, f);
    fclose(f);
}

void check(ada_analysis_unit unit)
{
    ada_base_entity ast_root;
    ada_base_entity prelude_list, with_clause;
    ada_base_entity has_limited;

    if (unit == NULL)
        error("Could not create the analysis unit for foo.adb from a file");
    ada_unit_root(unit, &ast_root);

    if (ada_node_is_null(&ast_root))
        dump_diagnostics(unit, "foo.adb");
    else {
        if (!ada_compilation_unit_f_prelude(ast_root.el, &no_entity_info,
                                            &prelude_list.el)
            || !ada_node_child(&prelude_list, 0, &with_clause)
            || !ada_with_clause_f_has_limited(with_clause.el, &no_entity_info,
                                              &has_limited.el))
            error("Could not traverse the AST as expected");
        ada_bool is_limited;
        ada_limited_node_p_as_bool (has_limited.el, &no_entity_info, &is_limited);
        printf("WithClause: is_limited = %s\n", is_limited ? "true" : "false");
    }
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    libadalang_initialize();
    ctx = ada_create_analysis_context("iso-8859-1", NULL);
    if (ctx == NULL)
        error("Could not create the analysis context");

    /* First work with the "limited" keyword.  */
    write_source(src_buffer_1);

    puts("1. Parsing source 1");
    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0, 0);
    check(unit);

    /* Now work without the "limited" keyword:
        2. getting the unit without reparsing should preserve is_limited;
        3. trying to reparse the deleted file should raise an error but
           preserve the unit's tree;
        4. getting the unit with reparsing should clear is_limited.  */
    write_source(src_buffer_2);

    puts("2. Parsing source 2 (reparse=false)");
    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 0, 0);
    check(unit);

    remove("foo.adb");

    puts("3. Parsing with deleted file (reparse=true)");
    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 1, 0);
    check(unit);

    write_source(src_buffer_2);

    puts("4. Parsing source 2 (reparse=true)");
    unit = ada_get_analysis_unit_from_file(ctx, "foo.adb", NULL, 1, 0);
    check(unit);

    write_source(src_buffer_1);

    /* Now restore the "limited" keyword in the soruce:
        5. reparsing the unit should work and set is_limited;
        6. reparsing the unit with a deleted file should wipe the AST and emit
           the corresponding diagnostics.  preserve the unit's tree. */

    puts("5. Reparsing source 1");
    ada_unit_reparse_from_file(unit, NULL);
    check(unit);

    remove("foo.adb");

    puts("6. Reparsing with deleted file");
    ada_unit_reparse_from_file(unit, NULL);
    check(unit);

    ada_destroy_analysis_context(ctx);
    puts("Done.");
    return 0;
}
