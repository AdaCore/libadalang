## vim: filetype=makoada

--  Start or_code

${pos} := -1;
${res} := ${_self.get_type().nullexpr()};
% for ctx in results:
    ${ctx.code}
    if ${ctx.pos_var_name} /= -1 then
        ${pos} := ${ctx.pos_var_name};
        ${res} := ${_self.get_type().name()} (${ctx.res_var_name});
        goto ${exit_label};
    end if;
% endfor
<<${exit_label}>>

--  End or_code
