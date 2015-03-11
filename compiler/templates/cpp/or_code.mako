${pos} = -1;
${res} = ${_self.get_type().nullexpr()};
% for ctx in results:
    ${ctx.code}
    if (${ctx.pos_var_name} != -1) {
        ${pos} = ${ctx.pos_var_name};
        ${res} = ${("(%s)" % typ) if typ != 'Token' else ''}(${ctx.res_var_name});
        goto ${exit_label};
    }
% endfor
${exit_label}:
