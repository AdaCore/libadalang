#ifndef LANGKIT_FIND_H
#define LANGKIT_FIND_H

#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

static void
find_node(ada_node *root, ada_node_kind_enum kind, ada_node *result_p)
{
    ada_node root_copy;
    unsigned i;
    unsigned count;

    memcpy(&root_copy, root, sizeof(root_copy));
    count = ada_node_children_count(&root_copy);

    if (ada_node_kind (&root_copy) == kind) {
        memcpy(result_p, &root_copy, sizeof(*result_p));
        return;
    }

    for (i = 0; i < count; ++i) {
        ada_node child;

        if (!ada_node_child(&root_copy, i, &child))
            error("Error while getting a child");

        if (ada_node_is_null(&child))
            continue;
        find_node(&child, kind, &child);
        if (!ada_node_is_null(&child)) {
            memcpy(result_p, &child, sizeof(*result_p));
            return;
        }
    }
    result_p->node = NULL;
}

#endif
