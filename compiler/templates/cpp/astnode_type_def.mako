## vim: filetype=makocpp

class ${cls.name()} : public ${base_name} {
protected:
    % if not cls.abstract:
        virtual ASTNode *lookup_children(const SourceLocation &sloc, bool snap=false);
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

    % if not cls.abstract:
        void validate();
        void print_node(int level = 0);
        boost::property_tree::ptree get_property_tree();

        unsigned get_child_count() const;
        bool get_child(unsigned index, ASTNode*& result);

        virtual ${node_kind.tagged} kind() {
            return ${capi.get_name(cls.name())};
        }

        void compute_indent_level();
    % endif
};

extern long ${cls.name().lower()}_counter;

% if not cls.is_ptr:
extern ${cls.name()} nil_${cls.name()};
% endif
