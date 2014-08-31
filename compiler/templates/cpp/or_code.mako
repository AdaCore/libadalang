${pos} = -1;
${res} = ${_self.get_type().nullexpr()};
% for mpos, mres, m_code, _ in results:
    ${m_code}
    if (${mpos} != -1) {
        ${pos} = ${mpos};
        ${res} = ${("(%s)" % typ) if typ != 'Token' else ''}(${mres});
        goto ${exit_label};
    }
% endfor
${exit_label}:
