## vim: filetype=makocpp

// Start transform_code

${parser_context.code}
% if _self.get_type().is_ptr:
    ${res} = ${_self.typ.nullexpr()};
% endif

if (${parser_context.pos_var_name} != -1) {
    % if not is_row(_self.parser) and _self.parser.needs_refcount():
        if (${parser_context.res_var_name}) ${parser_context.res_var_name}->inc_ref();
    % endif

    % if _self.get_type().is_ptr:
        ${res} = ${_self.typ.name()}_new();
    % endif

    ## Compute and set the sloc range for this AST node.
    ${res}->token_data = token_data;
    ${res}->token_start = ${pos_name};
    ${res}->token_end = (${parser_context.pos_var_name} == ${pos_name}) ? ${pos_name} : ${parser_context.pos_var_name} - 1;

    % for f, arg, typ in zip(_self.typ.get_fields(), args, _self.get_type().get_types(_compile_ctx)):
        ${res}${"->" if _self.get_type().is_ptr else "."}${f.name} = ${arg};
        % if is_ast_node (typ):
             if (${arg}) ${arg}->setParent(${res});
        % endif
    % endfor
}

// end transform_code
