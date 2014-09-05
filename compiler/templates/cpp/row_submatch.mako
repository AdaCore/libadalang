## vim: filetype=cpp

${m_code}

% if not parser.discard():
   ${subresult} = ${parser.get_type().nullexpr()};
% endif

if (${mpos} != -1) {
    % if parser.needs_refcount() and _self.components_need_inc_ref and not parser.discard():
        % if parser.get_type().is_ptr:
            if (${mres}) ${mres}->inc_ref();
        % else:
            ${mres}.inc_ref();
        % endif
    % endif

    % if pos != mpos:
        ${pos} = ${mpos};
    % endif

    % if not parser.discard():
        ${subresult} = ${mres};
    % endif

} else {
   ${pos} = -1;
   ${did_fail} = true;
   goto ${exit_label}_${i};
}
