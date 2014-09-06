## vim: filetype=cpp

class ${cls.name()} : public ${base_name} {
protected:
    % if not cls.abstract:
        virtual ASTNode *lookup_children(const SourceLocation &sloc);
    % endif

public:

    % for t, f in cls_field_decls:
         ${decl_type(t)} ${f.name};
    % endfor

    std::string repr();

    % if cls.fields:
        ${cls.name()}() : ${", ".join("{0}({1})".format(f.name, t.nullexpr()) for t, f in all_field_decls)} {}
    % endif

    std::string __name();
    ~${cls.name()}();
};

extern long ${cls.name().lower()}_counter;

% if not cls.is_ptr:
extern ${cls.name()} nil_${cls.name()};
% endif
