## vim: filetype=makocpp

typedef enum {
    ${capi.get_name("{}_{}".format(cls.name(), "uninitialized"))} = 0,
    ${", ".join("{} = {}".format(alt, i)
                for i, alt in enumerate(cls.c_alternatives(capi), 1))}
} ${cls.c_type(capi).name};
