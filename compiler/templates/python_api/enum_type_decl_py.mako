## vim: filetype=python

# Enumerators for ${cls.c_type(capi).name}
% for py_alt, alt in zip(cls.alternatives_for(pyapi), cls.alternatives):
${py_alt} = '${alt}'
% endfor
${cls.c_type(capi).name}_to_str = [
    UNINITIALIZED,
% for alt in cls.alternatives:
    '${alt}',
% endfor
]
