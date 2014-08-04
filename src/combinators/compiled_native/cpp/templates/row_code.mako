## vim: filetype=cpp

${pos} = ${pos_name};
${did_fail} = false;
${body}

goto ${exit_label}_0;
% for i in range(len(_self.matchers) - 1, -1, -1):
    ${exit_label}_${i}:
    % if i > 0 and _self.components_need_inc_ref and _self.matchers[i-1].needs_refcount() and not isinstance(_self.matchers[i-1], Discard):
        % if _self.matchers[i-1].is_ptr():
            if (${subresults[i-1]}) if (${subresults[i-1]}->dec_ref()) { ${subresults[i-1]} = nullptr; }
        % else:
            if (${subresults[i-1]}.dec_ref()) { ${subresults[i-1]} = nullptr; }
        % endif
    % endif
% endfor

if (!${did_fail}) {
    % if _self.make_tuple:
        % if _self.is_ptr():
            ${res} = new ${_self.type_name};
        % endif
        % for fid, arg in enumerate(_self.args):
            ${res}${"->" if _self.is_ptr() else "."}field_${fid} = ${arg};
        % endfor
        // ${res}${"->" if _self.is_ptr() else "."}inc_ref();
    % endif
}
