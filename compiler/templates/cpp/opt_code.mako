## vim: filetype=makocpp

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

    % if _self._is_error:
        ## Emit a diagnostic informing the user that the sub parser has not
        ## succeeded
        diagnostics.push_back(Diagnostic(
            "Missing '${_self.parser.val if is_tok(_self.parser) else repr(_self.parser)}'",
            lexer->get(${pos_name} - 1).sloc_range
        ));
    % endif

    ${parser_context.pos_var_name} = ${pos_name};
}
