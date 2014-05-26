enum class ${cls.name()} : char {
    ${", ".join(alt for alt in cls.alternatives)}, uninitialized
};

std::string enum_repr(${cls.name()} enum_inst);
