## vim: filetype=cpp

${pos} = ${pos_name};
${did_fail} = false;
${body}

goto ${exit_label}_0;
% for i in range(len(_self.parsers) - 1, -1, -1):
    ${exit_label}_${i}:
    % if i > 0 and _self.components_need_inc_ref and _self.parsers[i-1].needs_refcount() and not is_discard(_self.parsers[i-1]):
        % if _self.parsers[i-1].get_type().is_ptr:
            if (${subresults[i-1]}) if (${subresults[i-1]}->dec_ref()) { ${subresults[i-1]} = nullptr; }
        % else:
            if (${subresults[i-1]}.dec_ref()) { ${subresults[i-1]} = nullptr; }
        % endif
    % endif
% endfor

if (!${did_fail}) {
    % if _self.make_tuple:
        % if _self.get_type().is_ptr:
            ${res} = new ${_self.typ.as_string()};
        % endif
        % for fid, arg in enumerate(_self.args):
            ${res}${"->" if _self.get_type().is_ptr else "."}field_${fid} = ${arg};
        % endfor
    % endif
}
