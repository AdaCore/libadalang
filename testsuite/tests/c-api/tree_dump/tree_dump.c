#include <stdio.h>
#include <stdlib.h>
#include "libadalang.h"


static void
error(const char *msg)
{
    fputs(msg, stderr);
    exit(1);
}

static void
print_indent(int level)
{
    int i;
    for (i = 0; i < level; ++i)
        printf("| ");
}

static void
dump(ada_node node, int level)
{
    ada_node_kind_enum kind;
    char *kind_name;
    unsigned i, count;

    if (node == NULL) {
        print_indent(level);
        printf("<null node>\n");
        return;
    }

    kind = ada_node_kind(node);
    kind_name = ada_kind_name(kind);
    print_indent(level);
    printf("<%s>\n", kind_name);
    free(kind_name);

    count = ada_node_child_count(node);
    for (i = 0; i < count; ++i)
    {
        ada_node child;

        if (ada_node_child(node, i, &child) == 0)
            error("Error while getting a child");
        dump(child, level + 1);
    }
}

int
main(void)
{
    ada_analysis_context ctx;
    ada_analysis_unit unit;

    ctx = ada_create_analysis_context();
    if (ctx == NULL)
        error("Could not create the analysis context\n");

    unit = ada_create_analysis_unit_from_file(ctx, "foo.adb");
    if (unit == NULL)
        error("Could not create the analysis unit from foo.adb");

    dump(ada_unit_root(unit), 0);

    ada_destroy_analysis_context(ctx);
    puts("Done");
    return 0;
}
