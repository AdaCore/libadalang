% if _self.empty_valid:
${pos} = ${pos_name};
% else:
${pos} = -1;
% endif

% if _self.is_ptr():
% if _self.empty_valid:
${res} = new ASTList<${_self.parser.get_type_string()}>;
% else:
${res} = ${_self.nullexpr()};
% endif
% endif

${cpos} = ${pos_name};
while (true) {
${pcode}
    if (${ppos} == -1) break;

    % if _self.is_ptr() and not _self.empty_valid:
    if (${res} == ${_self.nullexpr()}) {
        ${res} = new ASTList<${_self.parser.get_type_string()}>;
    }
    % endif

    ${pos} = ${ppos};
% if cpos != ppos:
    ${cpos} = ${ppos};
% endif
    ${res}${"->" if _self.is_ptr() else "."}vec.push_back (${pres});

    % if _self.sep:
    ${sep_code}
    if (${sep_pos} != -1) { 
        ${cpos} = ${sep_pos}; 
    }
    else break;
    % endif
}
// shrink_capacity(${res});
