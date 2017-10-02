#ifndef LANGKIT_FIND_H
#define LANGKIT_FIND_H

#include <stdlib.h>
#include <string.h>

#include "libadalang.h"

static void
find_node(ada_base_entity *root, ada_node_kind_enum kind,
	  ada_base_entity *result_p)
{
    unsigned i;
    unsigned count = ada_node_child_count(root);

    if (ada_node_kind (root) == kind) {
        memcpy(result_p, root, sizeof(*result_p));
        return;
    }

    for (i = 0; i < count; ++i) {
        ada_base_entity child;

        if (!ada_node_child(root, i, &child))
            error("Error while getting a child");

        if (ada_node_is_null(&child))
            continue;
        find_node(&child, kind, &child);
        if (!ada_node_is_null(&child)) {
            memcpy(result_p, &child, sizeof(*result_p));
            return;
        }
    }
    result_p->el = NULL;
}

#endif
