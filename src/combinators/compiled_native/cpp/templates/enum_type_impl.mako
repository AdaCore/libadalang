## vim: filetype=cpp

std::string enum_repr(${cls.name()} enum_inst) {
    switch (enum_inst) {
% for alt in cls.alternatives:
    case ${cls.name()}::${alt}:
        return "${alt}";
% endfor
    case ${cls.name()}::uninitialized:
        return "uninitialized";
    }
}

void dec_ref (${cls.name()} self) {}
