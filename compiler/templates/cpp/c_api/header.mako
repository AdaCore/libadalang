## vim: filetype=makocpp

#ifndef ${capi.header_guard_id}
#define ${capi.header_guard_id}

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Context for all source analysis.  */
typedef void* ${analysis_context.simple};

/* Context for the analysis of a single compilation unit.  References are
   ref-counted.  */
typedef void* ${analysis_unit.simple};

/* Data type for all AST nodes.  AST nodes are assembled to make up a tree.
   See the AST node primitives below to inspect such trees.  References are
   ref-counted.  */
typedef void* ${node.simple};

/* Kind of AST nodes in parse trees.  */
enum ${node_kind.simple} {
    /* TODO: do we switch to UPPER_CASE for labels?  */
    /* TODO: do we keep a single node kind for all lists or should we
       specialize them?  */
    ${capi.get_name("list")} = 0,
% for astnode in _self.astnode_types:
    % if not astnode.abstract:
        ${capi.get_name(astnode.name())}
          = ${compile_ctx.node_kind_constants[astnode]},
    % endif
% endfor
};

/* Data type for tokens.  Tokens always come from AST node and have the same
   lifetime than the AST node they come from.  */
typedef void* ${token.simple};


/* Helper data structures for source location handling.  */

struct ${sloc.simple} {
    uint32_t line;
    uint16_t column;
};

struct ${sloc_range.simple} {
    ${sloc.tagged} start;
    ${sloc.tagged} end;
};


/* Analysis unit diagnostics.  */
struct ${diagnostic.simple} {
    ${sloc_range.tagged} sloc_range;
    /* Reference to the diagnostic message: it can become invalid (a dangling
       pointer) as soon as diagnostic are appended or removed from the
       corresponding analysis unit.  */
    const char *message;
};


/*
 * Data structures held in AST nodes
 */


% for chunk in _self.c_astnode_field_types.values():
    ${chunk}
%endfor

/* Get the text of the given token.  The returned string has the same lifetime
   than "token".  */
const char *${capi.get_name("token_text")} (${token.tagged} token);


/*
 * Analysis primitives
 */

/* Create and return an analysis context.  The caller is responsible to destroy
   it when done with it.  */
extern ${analysis_context.tagged}
${capi.get_name("create_analysis_context")}(void);

/* Destroy an analysis context.  Any analysis units it contains may survive if
   there are still references to it.  */
extern void
${capi.get_name("destroy_analysis_context")}(
        ${analysis_context.tagged} context);

/* Create an analysis unit out under this context from some source file.  At
   this point the returned analysis unit is owned only by the analysis
   context.  */
extern ${analysis_unit.tagged}
${capi.get_name("create_analysis_unit_from_file")}(
        ${analysis_context.tagged} context,
        const char *filename);

/* Remove the corresponding analysis unit from this context.  Note that if
   someone still owns a reference to this unit, it is still available.  */
extern void
${capi.get_name("remove_analysis_unit")}(${analysis_context.tagged} context,
                                         const char *filename);

/* Return the root AST node for this unit, or NULL if there is none.  */
extern ${node.tagged}
${capi.get_name("unit_root")}(${analysis_unit.tagged} unit);

/* Return the number of diagnostics associated to this unit.  */
extern unsigned
${capi.get_name("unit_diagnostic_count")}(${analysis_unit.tagged} unit);

/* Get the Nth diagnostic in this unit and store it into *DIAGNOSTIC_P.  Return
   zero on failure (when N is too big).  */
extern int
${capi.get_name("unit_diagnostic")}(${analysis_unit.tagged} unit,
                                    unsigned n,
                                    ${diagnostic.tagged} *diagnostic_p);

/* Increase the reference count to an analysis unit.  Return the reference for
   convenience.  */
extern ${analysis_unit.tagged}
${capi.get_name("unit_incref")}(${analysis_unit.tagged} unit);

/* Decrease the reference count to an analysis unit.  */
extern void
${capi.get_name("unit_decref")}(${analysis_unit.tagged} unit);


/*
 * General AST node primitives
 */

/* Get the kind of an AST node.  */
extern ${node_kind.tagged}
${capi.get_name("node_kind")}(${node.tagged} node);

/* Helper for textual dump: return the name of a node kind.  */
extern const char*
${capi.get_name("kind_name")}(${node_kind.tagged} kind);

/* Get the spanning source location range for an AST node.  */
extern void
${capi.get_name("node_sloc_range")}(${node.tagged} node,
                                    ${sloc_range.tagged} *sloc_range);

/* Return the bottom-most AST node from NODE that contains SLOC, or NULL if
   there is none.  */
extern ${node.tagged}
${capi.get_name("lookup_in_node")}(${node.tagged} node,
                                   const ${sloc.tagged} *sloc);

/* Return the lexical parent of NODE, if any.  Return NULL for the root AST
   node or for AST nodes for which no one has a reference to the parent.  */
extern ${node.tagged}
${capi.get_name("node_parent")}(${node.tagged} node);

/* Return the number of AST node in NODE's fields.  */
extern unsigned
${capi.get_name("node_child_count")}(${node.tagged} node);

/* Get the Nth child AST node in NODE's fields and store it into *CHILD_P.
   Return zero on failure (when N is too big).  */
extern int
${capi.get_name("node_child")}(${node.tagged} node,
                               unsigned n,
                               ${node.tagged}* child_p);

/* Increase the reference count to an AST node.  Return the reference for
   convenience.  */
extern ${node.tagged}
${capi.get_name("node_incref")}(${node.tagged} node);

/* Decrease the reference count to an AST node.  */
extern void
${capi.get_name("node_decref")}(${node.tagged} node);


/*
 * Kind-specific AST node primitives
 */

/* All these primitives return their result through an OUT parameter.  They
   return a boolean telling whether the operation was successful (it can fail
   if the node does not have the proper type, for instance).  When an AST node
   is returned, its ref-count is left as-is.  */

% for astnode in _self.astnode_types:
    % for primitive in _self.c_astnode_primitives[astnode]:
        ${primitive.declaration}
    % endfor
% endfor


/*
 * Extensions handling
 */

/* The following functions make it possible to attach arbitrary data to AST
   nodes: these are extensions.  Each data is associated with both an extension
   ID and a destructor.  AST nodes can have either none or only one extension
   for a given ID.  The destructor is called when the AST node is about to be
   destroyed itself.

   This mechanism is inteded to ease annotating trees with analysis data but
   also to host node wrappers for language bindings.  */

/* Type for extension destructors.  The parameter are the "node" the extension
   was attached to and the "extension" itself.  */
typedef void (*${capi.get_name("node_extension_destructor")})(${node.tagged} node,
                                                              void *extension);

/* Register an extension and return its identifier.  Multiple calls with the
   same name will return the same identifier.  */
extern unsigned
${capi.get_name("register_extension")}(const char *name);

/* Create an extension slot in "node".  If this node already contains an
   extension for "ext_id", return the existing slot.  If not, create such a
   slot, associate the "dtor" destructor to it and initialize the slot to NULL.
   Return a pointer to the slot.

   Note that the pointer is not guaranteed to stay valid after further calls to
   this function.  */
extern void **
${capi.get_name("node_extension")}(${node.tagged} node,
                                   unsigned ext_id,
                                   ${capi.get_name("node_extension_destructor")} dtor);

#ifdef __cplusplus
}
#endif

#endif
