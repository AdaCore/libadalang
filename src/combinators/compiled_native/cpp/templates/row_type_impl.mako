## vim: filetype=cpp

${_self.type_name} nil_${_self.type_name};

void ${_self.type_name}::inc_ref() { 
    ref++; 
#if DEBUG_MODE
    printf("%s", indent_str.c_str());
    printf("IN INC REF FOR ${_self.type_name}, REF = %d\n", ref);
#endif
}

bool ${_self.type_name}::dec_ref() {
    ref--;
#if DEBUG_MODE
    printf("%s", indent_str.c_str());
    printf("IN DEC REF FOR ${_self.type_name} NODE, REF AFTER DEC REF = %d\n", ref);
#endif
    if (ref <= 0) {
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
        % if _self.is_ptr():
            delete this;
        % endif
        return true;
    }
    return false;
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
