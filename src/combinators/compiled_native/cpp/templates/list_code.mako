## vim: filetype=cpp

% if _self.empty_valid:
    ${pos} = ${pos_name};
% else:
    ${pos} = -1;
% endif

% if _self.empty_valid:
    ${res} = new ASTList<${decl_type(_self.parser.get_type())}>;
% else:
    ${res} = ${_self.get_type().nullexpr()};
% endif

${cpos} = ${pos_name};

while (true) {
    ${pcode}
    if (${ppos} == -1) break;

    ${pos} = ${ppos};
    % if cpos != ppos:
        ${cpos} = ${ppos};
    % endif

    % if _self.revtree_class:
        if (${res} == ${_self.get_type().nullexpr()})
            ${res} = ${pres};
        else {
            auto new_res = ${_self.revtree_class.as_string()}_new();
            new_res->${_self.revtree_class.fields[0].name} = ${res};
            new_res->${_self.revtree_class.fields[1].name} = ${pres};
            ${res} = new_res;
        }
    % else:
        if (${res} == ${_self.get_type().nullexpr()}) {
            ${res} = new ASTList<${decl_type(_self.parser.get_type())}>;
        }
        ${res}${"->" if _self.get_type().is_ptr else "."}vec.push_back (${pres});
    % endif

    % if _self.parser.needs_refcount():
        % if _self.parser.get_type().is_ptr:
            if (${pres}) ${pres}->inc_ref();
        % else:
            ${pres}.inc_ref();
        % endif
    % endif

    % if _self.sep:
        ${sep_code}
        if (${sep_pos} != -1) { 
            ${cpos} = ${sep_pos}; 
        }
        else break;
    % endif
}
