## vim: filetype=cpp

extern int
${capi.get_name("{}_{}".format(astnode.name(), field.name))}(
        ${capi.node_type.tagged_name} node,
        ${field_type.c_type(capi).tagged_name} *value_p);
