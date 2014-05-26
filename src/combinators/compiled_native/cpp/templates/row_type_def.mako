class ${_self.type_name} : ASTNode {
public:
% for i, m in enumerate(matchers):
   ${m.get_type_string()} field_${i};
% endfor
    std::string repr();

    ${_self.type_name}() : ${", ".join("field_{0}({1})".format(i, m.nullexpr()) for i, m in enumerate(matchers))} {}

};

extern ${_self.type_name} nil_${_self.type_name};
