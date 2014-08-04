## vim: filetype=cpp

${m_code}

% if not is_discard:
   ${subresult} = ${matcher.nullexpr()};
% endif

if (${mpos} != -1) {
    // TYPE : ${type(matcher)}
    // TYPE : ${matcher.get_type()}
    // IS_PTR : ${matcher.is_ptr()}
    % if matcher.needs_refcount() and _self.components_need_inc_ref and not is_discard:
        % if matcher.is_ptr():
            if (${mres}) ${mres}->inc_ref();
        % else:
            ${mres}.inc_ref();
        % endif
    % endif

    % if pos != mpos:
        ${pos} = ${mpos};
    % endif

    % if not is_discard:
        ${subresult} = ${mres};
    % endif

} else {
   ${pos} = -1;
   ${did_fail} = true;
   goto ${exit_label}_${i};
}
