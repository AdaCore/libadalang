## vim: filetype=makocpp

typedef enum {
    ${capi.get_enum_alternative(
        cls.base_name(), Name("Uninitialized"), cls.suffix
    )} = 0,
    ${", ".join("{} = {}".format(alt, i)
                for i, alt in enumerate(cls.alternatives_for(capi), 1))}
} ${cls.c_type(capi).name};
