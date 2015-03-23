## vim: filetype=makocpp

${parser_context.code}

% if not parser.discard():
   ${subresult} = ${parser.get_type().nullexpr()};
% endif

if (${parser_context.pos_var_name} != -1) {
    % if parser.needs_refcount() and _self.components_need_inc_ref and not parser.discard():
        % if parser.get_type().is_ptr:
            if (${parser_context.res_var_name}) ${parser_context.res_var_name}->inc_ref();
        % else:
            ${parser_context.res_var_name}.inc_ref();
        % endif
    % endif

    % if pos != parser_context.pos_var_name:
        ${pos} = ${parser_context.pos_var_name};
    % endif

    % if not parser.discard():
        ${subresult} = ${parser_context.res_var_name};
    % endif

} else {
   ${pos} = -1;
   ${did_fail} = true;
   goto ${exit_label}_${i};
}
