${pos} = ${pos_name};
${did_fail} = false;
${body}
${exit_label}:

% if _self.make_tuple:
if (!${did_fail}) {
    //${res} = new ${_self.type_name};
    % for fid, arg in enumerate(_self.args):
    ${res}.field_${fid} = ${arg};
    % endfor
}
% endif
