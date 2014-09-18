## vim: filetype=cpp

${start_sloc_range_var} = get(lex, ${pos_name}).sloc_range;

${code}
% if _self.get_type().is_ptr:
    ${res} = ${_self.typ.nullexpr()};
% endif

if (${cpos} != -1) {
    % if not is_row(_self.parser) and _self.parser.needs_refcount:
        if (${cres}) ${cres}->inc_ref();
    % endif

    % if _self.get_type().is_ptr:
        ${res} = ${_self.typ.name()}_new();
    % endif

    ## Compute and set the sloc range for this AST node.
    ${res}${"->" if _self.get_type().is_ptr else "."}sloc_range_ =
        SourceLocationRange(${start_sloc_range_var}.get_start(),
                            (${cpos} == ${pos_name})
                            ? ${start_sloc_range_var}.get_end()
                            : get(lex, ${cpos} - 1).sloc_range.get_end());

    % for f, arg in zip(_self.typ.get_fields(), args):
        ${res}${"->" if _self.get_type().is_ptr else "."}${f.name} = ${arg}; 
    % endfor
}
