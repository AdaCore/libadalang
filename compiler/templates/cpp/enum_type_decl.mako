## vim: filetype=makocpp

enum class ${cls.name()} : char {
    ## Assign explicit constants members to ease binding to other languages.
    ## Start regular enumerators at 1 so that uninitialized is always 0, which
    ## is kind of consistent with other "absent" values such as the NULL pointer,
    ## etc.
    uninitialized = 0,
    ${", ".join("{} = {}".format(alt, i)
                for i, alt in enumerate(cls.alternatives, 1))}
};

void dec_ref (${cls.name()} self);

std::string get_repr(${cls.name()} enum_inst);
boost::property_tree::ptree get_ptree(${cls.name()} enum_inst);
