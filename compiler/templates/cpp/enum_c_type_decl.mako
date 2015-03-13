## vim: filetype=cpp

enum ${cls.c_type(capi).name} {
    ${capi.get_name("{}_{}".format(cls.name(), "uninitialized"))} = 0,
    ${", ".join("{} = {}".format(alt, i)
                for i, alt in enumerate(cls.c_alternatives(capi), 1))}
};
