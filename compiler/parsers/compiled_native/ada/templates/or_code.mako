${pos} := -1;
${res} := ${"NoToken" if typ == "Token" else "null"};
% for mpos, mres, m_code, _ in results:
    ${m_code}
    if ${mpos} /= -1 then
    ${pos} := ${mpos};
    ${res} := ${typ if typ != 'Token' else ''} (${mres});
    goto ${exit_label};
    end if;
% endfor
<<${exit_label}>>
