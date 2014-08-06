## vim: filetype=cpp

class ${_self.type_name} : public ASTNode {
protected:
public:
% for i, m in enumerate(matchers):
   ${m.get_type_string()} field_${i};
% endfor
    std::string repr();
    ${_self.type_name}() : ${", ".join("field_{0}({1})".format(i, m.nullexpr()) for i, m in enumerate(matchers))} {}

    ~${_self.type_name}();

};

extern ${_self.type_name} nil_${_self.type_name};
