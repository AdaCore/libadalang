#ifndef LANGKIT_DUMP_H
#define LANGKIT_DUMP_H

#include <stdbool.h>
#include <stdio.h>

#include "libadalang.h"

#include "langkit_text.h"
#include "utils.h"


static void
print_indent(int level)
{
    int i;
    for (i = 0; i < level; ++i)
        printf("| ");
}

static void
print_sloc_range(ada_source_location_range *sr)
{
    printf("%d:%d-%d:%d",
           sr->start.line, sr->start.column,
           sr->end.line, sr->end.column);
}

static void
dump(ada_base_node node, int level)
{
    ada_node_kind_enum kind;
    ada_text kind_name;
    unsigned i, count;

    if (node == NULL) {
        print_indent(level);
        printf("<null node>\n");
        return;
    }

    kind = ada_node_kind(node);
    kind_name = ada_kind_name(kind);
    print_indent(level);
    putchar('<');
    fprint_text(stdout, kind_name, false);
    puts(">");

    count = ada_node_child_count(node);
    for (i = 0; i < count; ++i)
    {
        ada_base_node child;

        if (ada_node_child(node, i, &child) == 0)
            error("Error while getting a child");
        dump(child, level + 1);
    }
}

static void
dump_diagnostics(ada_analysis_unit unit, const char *unit_name)
{
    unsigned i;

    printf("Diagnostics for %s\n", unit_name);
    for (i = 0; i < ada_unit_diagnostic_count(unit); ++i) {
        ada_diagnostic d;

        if (!ada_unit_diagnostic(unit, i, &d))
            error("Error while getting a diagnostic");
        printf("  ");
        if (d.sloc_range.start.line != 0) {
            print_sloc_range(&d.sloc_range);
            printf(": ");
        }
        fprint_text(stdout, d.message, false);
        putchar('\n');
    }
}

#endif /* LANGKIT_DUMP_H */
