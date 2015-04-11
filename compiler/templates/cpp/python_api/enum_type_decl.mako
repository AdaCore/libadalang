## vim: filetype=python

# Enumerators for ${cls.c_type(capi).simple}
% for c_alt, alt in zip(cls.c_alternatives(capi), cls.alternatives):
${alt.upper()} = '${alt}'
% endfor
${cls.c_type(capi).simple}_to_str = [
    UNINITIALIZED,
% for alt in cls.alternatives:
    '${alt}',
% endfor
]
