% if not is_discard:
   ${subresult} = ${matcher.nullexpr()};
% endif

if (${mpos} != -1) {
    % if pos != mpos:
    ${pos} = ${mpos};
    % endif
% if not is_discard:
   ${subresult} = ${mres};
% endif
} else {
   ${pos} = -1;
   ${did_fail} = true;
   goto ${exit_label};
}
