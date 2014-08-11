## vim: filetype=cpp

class ${_self.typ.as_string()} : public ASTNode {
protected:
public:
% for i, m in enumerate(matchers):
   ${decl_type(m.get_type())} field_${i};
% endfor
    std::string repr();
    ${_self.typ.as_string()}() : ${", ".join("field_{0}({1})".format(i, m.get_type().nullexpr()) for i, m in enumerate(matchers))} {}

    ~${_self.typ.as_string()}();

};
