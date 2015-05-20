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
main(void)
{
    int i;
    const char *buffers[2];
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    buffers[0] = src_buffer_1;
    buffers[1] = src_buffer_2;

    ctx = ada_create_analysis_context();
    if (ctx == NULL)
        error("Could not create the analysis context");

    for (i = 0; i < 2; ++i)
    {
        ada_node prelude_list, with_decl;
        int is_limited;
        unit = ada_get_analysis_unit_from_buffer(ctx, "foo.adb",
                                                 buffers[i],
                                                 strlen(buffers[i]));
        if (unit == NULL)
            error("Could not create the analysis unit for foo.adb from a"
                  " buffer");

        if (!ada_compilation_unit_prelude(ada_unit_root(unit), &prelude_list)
            || !ada_node_child(prelude_list, 0, &with_decl)
            || !ada_with_decl_is_limited(with_decl, &is_limited))
            error("Could not traverse the AST as expected");
        printf("WithDecl: is_limited = %s\n", is_limited ? "true" : "false");
    }

    ada_destroy_analysis_context(ctx);
    puts("Done.");
    return 0;
}
