## vim: filetype=makoada

--  Start opt_code

${parser_context.code}

% if _self._booleanize:
    ${bool_res} := True;
% endif

if ${parser_context.pos_var_name} = -1 then
    % if _self._booleanize:
        ${bool_res} := False;
    % else:
        ${parser_context.res_var_name} := ${_self.parser.get_type().nullexpr()};
    % endif

    % if _self._is_error:
        ## Emit a diagnostic informing the user that the sub parser has not
        ## succeeded
        Parser.Diagnostics.Append
          ((Get_Token (Parser.TDH.all, ${pos_name}).Sloc_Range,
            To_Unbounded_String
            ("Missing '${_self.parser.val if is_tok(_self.parser) else repr(_self.parser)}'")));
    % endif

    ${parser_context.pos_var_name} := ${pos_name};
end if;

--  End opt_code
