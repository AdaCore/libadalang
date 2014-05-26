${code}
% if _self.is_ptr():
${res} = ${_self.typ.nullexpr()};
% endif
if (${cpos} != -1) {
    % if _self.is_ptr():
    ${res} = ${_self.typ.name()}_new();
    % endif
    % for f, arg in zip(_self.typ.get_fields(), args):
    ${res}${"->" if _self.is_ptr() else "."}${f.name} = ${arg}; 
    % endfor
}
