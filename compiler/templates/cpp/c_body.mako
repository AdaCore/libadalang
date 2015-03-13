## vim: filetype=cpp

#include "${capi.lib_name}.h"
#include "c_utils.hpp"
#include "lexer.hpp"
#include "ast.hpp"
#include "parse.hpp"


/*
 * Analysis primitives
 */

${capi.analysis_context_type.tagged_name}
${capi.get_name("create_analysis_context")}(void) {
    return (new AnalysisContext())->wrap();
}

void
${capi.get_name("destroy_analysis_context")}(
        ${capi.analysis_context_type.tagged_name} context) {
    delete unwrap<AnalysisContext>(context);
}

${capi.analysis_unit_type.tagged_name}
${capi.get_name("create_analysis_unit_from_file")}(
        ${capi.analysis_context_type.tagged_name} context,
        const char *filename) {
    AnalysisContext *context_ = unwrap<AnalysisContext>(context);
    AnalysisUnit *unit_ = context_->create_from_file(filename);
    return unit_->wrap();
}

void
${capi.get_name("remove_analysis_unit")}(
        ${capi.analysis_context_type.tagged_name} context,
        const char *filename) {
    AnalysisContext *context_ = unwrap<AnalysisContext>(context);
    context_->remove(filename);
}

${capi.node_type.tagged_name}
${capi.get_name("unit_root")}(
        ${capi.analysis_unit_type.tagged_name} unit) {
    AnalysisUnit *unit_ = unwrap<AnalysisUnit>(unit);
    return unit_->ast_root->wrap();
}


/*
 * General AST node primitives
 */

static const char* node_kind_names[] = {
    "list",
% for astnode in _self.astnode_types:
    % if not astnode.abstract:
        "${astnode.name()}",
    % endif
% endfor
};

${capi.node_kind_type.tagged_name}
${capi.get_name("node_kind")}(${capi.node_type.tagged_name} node) {
    return unwrap<ASTNode>(node)->kind();
}

const char*
${capi.get_name("kind_name")}(
        ${capi.node_kind_type.tagged_name} kind) {
    return node_kind_names[kind];
}

void
${capi.get_name("node_sloc_range")}(
        ${capi.node_type.tagged_name} node,
        ${capi.sloc_range_type.tagged_name} *sloc_range) {
    *sloc_range = wrap(unwrap<ASTNode>(node)->sloc_range_);
}

${capi.node_type.tagged_name}
${capi.get_name("lookup_in_node")}(${capi.node_type.tagged_name} node,
                                   const ${capi.sloc_type.tagged_name} *sloc) {
    ASTNode *result = unwrap<ASTNode>(node)->lookup(unwrap (*sloc));
    return result->wrap();
}

${capi.node_type.tagged_name}
${capi.get_name("node_parent")}(${capi.node_type.tagged_name} node) {
    ASTNode *result = unwrap<ASTNode>(node)->parent();
    return result->wrap();
}

unsigned
${capi.get_name("node_child_count")}(${capi.node_type.tagged_name} node) {
    return unwrap<ASTNode>(node)->get_children().size();
}

extern int
${capi.get_name("node_child")}(${capi.node_type.tagged_name} node,
                               unsigned n,
                               ${capi.node_type.tagged_name}* child_p) {
    std::vector<ASTNode*> children
      = unwrap<ASTNode>(node)->get_children();
    if (n >= children.size())
        return 0;
    else {
        *child_p = children[n]->wrap();
        return 1;
    }
}


/*
 * Kind-specific AST node primitives
 */

% for astnode in _self.astnode_types:
    % for primitive in _self.c_astnode_primitives[astnode]:
        ${primitive.implementation}
    % endfor
% endfor
