## vim: filetype=cpp

${_self.type_name} nil_${_self.type_name};

${_self.type_name}::~${_self.type_name}() {
        % for i, m in enumerate(matchers):
            % if m.is_ptr():
                if (field_${i}) {
                    field_${i}->dec_ref();
                    field_${i} = nullptr;
                }
            % endif
        % endfor
    #if DEBUG_MODE
        printf("DELETING ROW ${_self.type_name}\n");
    #endif
}

std::string ${_self.type_name}::repr() {
    std::string res = "(";
    % for i, m in enumerate(matchers):
    res.append(${m.emit_repr("this->field_%d" % i)});
    % if i < len(matchers) - 1:
    res.append(", ");
    % endif
    % endfor
    res.append(")");
    return res;
}
