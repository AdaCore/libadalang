## vim: filetype=cpp

${_self.typ.as_string()}::~${_self.typ.as_string()}() {
        % for i, m in enumerate(parsers):
            % if m.get_type().is_ptr:
                if (field_${i}) {
                    field_${i}->dec_ref();
                    field_${i} = nullptr;
                }
            % endif
        % endfor
    #if DEBUG_MODE
        printf("DELETING ROW ${decl_type(_self.typ)}\n");
    #endif
}

std::string ${_self.typ.as_string()}::repr() {
    std::string res = "(";
    % for i, m in enumerate(parsers):
    res.append(get_repr(this->field_${i}));
    % if i < len(parsers) - 1:
    res.append(", ");
    % endif
    % endfor
    res.append(")");
    return res;
}
