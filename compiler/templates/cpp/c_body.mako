## vim: filetype=makocpp

#include "${capi.lib_name}.h"
#include "c_utils.hpp"
#include "lexer.hpp"
#include "ast.hpp"
#include "parse.hpp"
#include "extensions.hpp"


/*
 * Analysis primitives
 */

${analysis_context.tagged}
${capi.get_name("create_analysis_context")}(void) {
    return (new AnalysisContext())->wrap();
}

void
${capi.get_name("destroy_analysis_context")}(
        ${analysis_context.tagged} context) {
    delete unwrap<AnalysisContext>(context);
}

${analysis_unit.tagged}
${capi.get_name("create_analysis_unit_from_file")}(
        ${analysis_context.tagged} context,
        const char *filename) {
    AnalysisContext *context_ = unwrap<AnalysisContext>(context);
    AnalysisUnit *unit_ = context_->create_from_file(filename);
    return unit_->wrap();
}

void
${capi.get_name("remove_analysis_unit")}(${analysis_context.tagged} context,
                                         const char *filename) {
    AnalysisContext *context_ = unwrap<AnalysisContext>(context);
    context_->remove(filename);
}

${node.tagged}
${capi.get_name("unit_root")}(${analysis_unit.tagged} unit) {
    AnalysisUnit *unit_ = unwrap<AnalysisUnit>(unit);
    return unit_->ast_root->wrap();
}

unsigned
${capi.get_name("unit_diagnostic_count")}(${analysis_unit.tagged} unit) {
    AnalysisUnit *unit_ = unwrap<AnalysisUnit>(unit);
    return unit_->diagnostics.size();
}

int
${capi.get_name("unit_diagnostic")}(${analysis_unit.tagged} unit,
                                    unsigned n,
                                    ${diagnostic.tagged} *diagnostic_p) {
    AnalysisUnit *unit_ = unwrap<AnalysisUnit>(unit);
    auto& diagnostics = unit_->diagnostics;
    if (n < diagnostics.size())
    {
        auto& d = diagnostics[n];
        diagnostic_p->sloc_range = wrap(d.sloc_range);
        diagnostic_p->message = d.raw_message.c_str();
        return 1;
    }
    return 0;
}

${analysis_unit.tagged}
${capi.get_name("unit_incref")}(${analysis_unit.tagged} unit) {
    AnalysisUnit *unit_ = unwrap<AnalysisUnit>(unit);
    unit_->inc_ref();
    return unit;
}

void
${capi.get_name("unit_decref")}(${analysis_unit.tagged} unit) {
    AnalysisUnit *unit_ = unwrap<AnalysisUnit>(unit);
    unit_->dec_ref();
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

${node_kind.tagged}
${capi.get_name("node_kind")}(${node.tagged} node) {
    return unwrap<ASTNode>(node)->kind();
}

const char*
${capi.get_name("kind_name")}(
        ${node_kind.tagged} kind) {
    return node_kind_names[kind];
}

void
${capi.get_name("node_sloc_range")}(${node.tagged} node,
                                    ${sloc_range.tagged} *sloc_range) {
    *sloc_range = wrap(unwrap<ASTNode>(node)->get_sloc_range());
}

${node.tagged}
${capi.get_name("lookup_in_node")}(${node.tagged} node,
                                   const ${sloc.tagged} *sloc) {
    ASTNode *result = unwrap<ASTNode>(node)->lookup(unwrap (*sloc));
    return result->wrap();
}

${node.tagged}
${capi.get_name("node_parent")}(${node.tagged} node) {
    ASTNode *result = unwrap<ASTNode>(node)->parent();
    return result->wrap();
}

unsigned
${capi.get_name("node_child_count")}(${node.tagged} node) {
    return unwrap<ASTNode>(node)->get_child_count();
}

extern int
${capi.get_name("node_child")}(${node.tagged} node,
                               unsigned n,
                               ${node.tagged}* child_p) {
    ASTNode *result;
    const bool success = unwrap<ASTNode>(node)->get_child(n, result);
    if (success)
        *child_p = result->wrap();
    return success;
}

${node.tagged}
${capi.get_name("node_incref")}(${node.tagged} node) {
    ASTNode *node_ = unwrap<ASTNode>(node);
    node_->inc_ref();
    return node;
}

void
${capi.get_name("node_decref")}(${node.tagged} node) {
    ASTNode *node_ = unwrap<ASTNode>(node);
    node_->dec_ref();
}


/*
 * Kind-specific AST node primitives
 */

% for astnode in _self.astnode_types:
    % for primitive in _self.c_astnode_primitives[astnode]:
        ${primitive.implementation}
    % endfor
% endfor


/*
 * Extensions handling
 */

unsigned
${capi.get_name("register_extension")}(const char *name) {
    return register_extension(name);
}

void **
${capi.get_name("node_extension")}(${node.tagged} node,
                                   unsigned ext_id,
                                   ${capi.get_name("node_extension_destructor")} dtor) {
    ASTNode *node_ = unwrap<ASTNode>(node);
    ASTNodeExtensionDestructor dtor_(dtor);
    return node_->get_extension(ext_id, dtor_);
}
