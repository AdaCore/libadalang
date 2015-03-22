## vim: filetype=makocpp

int
${accessor_name}(${node.tagged} node,
                 ${field_type.c_type(capi).tagged} *value_p) {
    ASTNode* node_ = unwrap<ASTNode>(node);
    ${astnode.name()}* typed_node = dynamic_cast<${astnode.name()}*>(node_);

    if (typed_node == nullptr)
        /* This primitive is invalid on this kind of node.  */
        return 0;
    else {
        % if is_enum(field_type):
            *value_p = (${field_type.c_type(capi).tagged}) typed_node->${field.name};
        % elif is_ast_node(field_type):
            *value_p = typed_node->${field.name}->wrap();
        % else:
            *value_p = typed_node->${field.name};
        % endif
        return 1;
    }
}
