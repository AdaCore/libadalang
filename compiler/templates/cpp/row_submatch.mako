## vim: filetype=cpp

${m_code}

% if not is_discard(parser):
   ${subresult} = ${parser.get_type().nullexpr()};
% endif

if (${mpos} != -1) {
    % if parser.needs_refcount() and _self.components_need_inc_ref and not is_discard(parser):
        % if parser.get_type().is_ptr:
            if (${mres}) ${mres}->inc_ref();
        % else:
            ${mres}.inc_ref();
        % endif
    % endif

    % if pos != mpos:
        ${pos} = ${mpos};
    % endif

    % if not is_discard(parser):
        ${subresult} = ${mres};
    % endif

} else {
   ${pos} = -1;
   ${did_fail} = true;
   goto ${exit_label}_${i};
}
