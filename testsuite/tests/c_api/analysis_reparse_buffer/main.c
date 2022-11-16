#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libadalang.h"

#include "utils.h"


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

void check(ada_analysis_unit unit)
{
    ada_base_entity root, prelude_list, with_clause;
    ada_base_entity has_limited;

    if (unit == NULL)
        error("Could not create the analysis unit for foo.adb from a buffer");

    ada_unit_root(unit, &root);
    if (!ada_compilation_unit_f_prelude(&root, &prelude_list)
        || !ada_node_child(&prelude_list, 0, &with_clause)
        || !ada_with_clause_f_has_limited(&with_clause, &has_limited))
        error("Could not traverse the AST as expected");

    ada_bool is_limited;
    ada_limited_node_p_as_bool (&has_limited, &is_limited);
    printf("WithClause: is_limited = %s\n", is_limited ? "true" : "false");
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    const size_t src_buffer_1_length = strlen(src_buffer_1);
    const size_t src_buffer_2_length = strlen(src_buffer_2);

    ctx = ada_allocate_analysis_context ();
    abort_on_exception ();

    ada_initialize_analysis_context (ctx, NULL, NULL, NULL, NULL, 1, 8);
    abort_on_exception ();

    /* Make sure the first parsing (with the "limited" keyword) works properly
       and check is_limited.  */
    puts("1. Parsing using buffer 1");
    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL,
                                             src_buffer_1,
                                             src_buffer_1_length,
                                             ada_default_grammar_rule);
    check(unit);

    /* Now make sure getting the unit with reparsing (without the "limited"
       keyword) clears is_limited.  */
    puts("2. Reparsing from context using buffer 2");
    unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb", NULL,
                                             src_buffer_2,
                                             src_buffer_2_length,
                                             ada_default_grammar_rule);
    check(unit);

    /* Finally make sure reparsing the unit (with the "limited" keyword) sets
       is_limited.  */
    puts("3. Reparsing from unit using buffer 1");
    ada_unit_reparse_from_buffer(unit, NULL,
                                 src_buffer_1, src_buffer_1_length);
    check(unit);

    ada_context_decref(ctx);
    puts("Done.");
    return 0;
}
