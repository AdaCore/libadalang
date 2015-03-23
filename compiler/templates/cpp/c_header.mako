## vim: filetype=makocpp

#ifndef ${capi.header_guard_id}
#define ${capi.header_guard_id}

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Context for all source analysis.  */
typedef void* ${capi.analysis_context_type.name};

/* Context for the analysis of a single compilation unit.  */
typedef void* ${capi.analysis_unit_type.name};

/* Data type for all AST nodes.  AST nodes are assembled to make up a tree.
   See the AST node primitives below to inspect such trees.  */
typedef void* ${capi.node_type.name};

/* Kind of AST nodes in parse trees.  */
enum ${capi.node_kind_type.name} {
    /* TODO: do we switch to UPPER_CASE for labels?  */
    /* TODO: do we keep a single node kind for all lists or should we
       specialize them?  */
    ${capi.get_name("list")},
% for astnode in _self.astnode_types:
    % if not astnode.abstract:
        ${capi.get_name(astnode.name())},
    % endif
% endfor
};


/* Helper data structures for source location handling.  */

struct ${capi.sloc_type.name} {
    uint32_t line;
    uint16_t column;
};

struct ${capi.sloc_range_type.name} {
    ${capi.sloc_type.tagged_name} start;
    ${capi.sloc_type.tagged_name} end;
};


/*
 * Data structures held in AST nodes
 */


% for chunk in _self.c_astnode_field_types.values():
    ${chunk}
%endfor


/*
 * Analysis primitives
 */

extern ${capi.analysis_context_type.tagged_name}
${capi.get_name("create_analysis_context")}(void);

extern void
${capi.get_name("destroy_analysis_context")}(
        ${capi.analysis_context_type.tagged_name} context);

extern ${capi.analysis_unit_type.tagged_name}
${capi.get_name("create_analysis_unit_from_file")}(
        ${capi.analysis_context_type.tagged_name} context,
        const char *filename);

extern void
${capi.get_name("remove_analysis_unit")}(
        ${capi.analysis_context_type.tagged_name} context,
        const char *filename);

extern ${capi.node_type.tagged_name}
${capi.get_name("unit_root")}(
        ${capi.analysis_unit_type.tagged_name} unit);


/*
 * General AST node primitives
 */

/* Get the kind of an AST node.  */
extern ${capi.node_kind_type.tagged_name}
${capi.get_name("node_kind")}(${capi.node_type.tagged_name} node);

/* Helper for textual dump: return the name of a node kind.  */
extern const char*
${capi.get_name("kind_name")}(${capi.node_kind_type.tagged_name} kind);

/* Get the spanning source location range for an AST node.  */
extern void
${capi.get_name("node_sloc_range")}(${capi.node_type.tagged_name} node,
                                        ${capi.sloc_range_type.tagged_name} *sloc_range);

/* Return the bottom-most AST node from NODE that contains SLOC, or NULL if
   there is none.  */
extern ${capi.node_type.tagged_name}
${capi.get_name("lookup_in_node")}(${capi.node_type.tagged_name} node,
                                   const ${capi.sloc_type.tagged_name} *sloc);

/* Return the lexical parent of NODE, if any.  Return NULL for the root AST
   node.  */
extern ${capi.node_type.tagged_name}
${capi.get_name("node_parent")}(${capi.node_type.tagged_name} node);

/* Return the number of AST node in NODE's fields.  */
extern unsigned
${capi.get_name("node_child_count")}(${capi.node_type.tagged_name} node);

/* Get the Nth child AST node in NODE's fields and store it in *CHILD_P.
   Return zero on failure (when N is too big).  */
extern int
${capi.get_name("node_child")}(${capi.node_type.tagged_name} node,
                               unsigned n,
                               ${capi.node_type.tagged_name}* child_p);


/*
 * Kind-specific AST node primitives
 */

/* All these primitives return their result through an OUT parameter.  They
   return a boolean telling whether the operation was successful (it can fail
   if the node does not have the proper type, for instance).  */

% for astnode in _self.astnode_types:
    % for primitive in _self.c_astnode_primitives[astnode]:
        ${primitive.declaration}
    % endfor
% endfor

#ifdef __cplusplus
}
#endif

#endif
