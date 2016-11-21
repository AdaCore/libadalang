#ifndef LANGKIT_FIND_H
#define LANGKIT_FIND_H

#include <stdlib.h>

#include "libadalang.h"

static ada_base_node
find_node(ada_base_node root, ada_node_kind_enum kind)
{
    unsigned i;
    unsigned count = ada_node_child_count(root);

    if (ada_node_kind (root) == kind)
      return root;

    for (i = 0; i < count; ++i) {
        ada_base_node child;

        if (!ada_node_child (root, i, &child))
            error("Error while getting a child");

        if (child == NULL)
            continue;
        child = find_node(child, kind);
        if (child != NULL)
            return child;
    }
    return NULL;
}

#endif
