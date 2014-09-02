## vim: filetype=cpp

${code}

% if _self._booleanize:
    ${bool_res} = true;
% endif

if (${mpos} == -1) {
    % if _self._booleanize:
        ${bool_res} = false;
    % else:
        ${mres} = ${_self.parser.get_type().nullexpr()};
    % endif
    ${mpos} = ${pos_name};
}
