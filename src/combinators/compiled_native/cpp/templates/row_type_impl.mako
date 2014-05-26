${_self.type_name} nil_${_self.type_name};

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
