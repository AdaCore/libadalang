## vim: filetype=makocpp
int
${accessor_name}(${node_type} node,
                 ${field_type.c_type(capi).name} *value_p) {
    ASTNode* node_ = unwrap<ASTNode>(node);
    ${astnode.name()}* typed_node = dynamic_cast<${astnode.name()}*>(node_);

    if (typed_node == nullptr)
        /* This primitive is invalid on this kind of node.  */
        return 0;
    else {
        % if is_enum(field_type):
            *value_p = (${field_type.c_type(capi).name}) typed_node->${field.code_name};
        % elif is_ast_node(field_type):
            *value_p = typed_node->${field.code_name}->wrap();
        % elif is_token_type(field_type):
            *value_p = &typed_node->${field.code_name};
        % else:
            *value_p = typed_node->${field.code_name};
        % endif
        return 1;
    }
}
