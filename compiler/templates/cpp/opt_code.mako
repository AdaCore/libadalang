## vim: filetype=cpp

${parser_context.code}

% if _self._booleanize:
    ${bool_res} = true;
% endif

if (${parser_context.pos_var_name} == -1) {
    % if _self._booleanize:
        ${bool_res} = false;
    % else:
        ${parser_context.res_var_name} = ${_self.parser.get_type().nullexpr()};
    % endif
    ${parser_context.pos_var_name} = ${pos_name};
}
