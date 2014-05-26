${code}
% if _self._booleanize:
${bool_res} = true;
% endif
if (${mpos} == -1) {
    % if _self._booleanize:
    ${bool_res} = false;
    % endif
    ${mpos} = ${pos_name};
    ${mres} = ${_self.matcher.nullexpr()};
}
