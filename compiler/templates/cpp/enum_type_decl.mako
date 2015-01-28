## vim: filetype=cpp

enum class ${cls.name()} : char {
    ${", ".join(alt for alt in cls.alternatives)}, uninitialized
};

void dec_ref (${cls.name()} self);

std::string get_repr(${cls.name()} enum_inst);
boost::property_tree::ptree get_ptree(${cls.name()} enum_inst);
