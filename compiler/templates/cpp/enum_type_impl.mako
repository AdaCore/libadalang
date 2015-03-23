## vim: filetype=makocpp

std::string get_repr(${cls.name()} enum_inst) {
    switch (enum_inst) {
% for alt in cls.alternatives:
    case ${cls.name()}::${alt}:
        return "${alt}";
% endfor
    case ${cls.name()}::uninitialized:
        return "uninitialized";
    }
}

boost::property_tree::ptree get_ptree(${cls.name()} enum_inst) {
    ptree result;
    result.put("", get_repr(enum_inst));
    return result;
}

void dec_ref (${cls.name()} self) {
    #pragma unused(self)
}
