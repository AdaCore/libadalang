#ifndef LANGKIT_DUMP_H
#define LANGKIT_DUMP_H

#include <stdbool.h>
#include <stdio.h>

#include "libadalang.h"

#include "utils.h"


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

#endif /* LANGKIT_DUMP_H */
