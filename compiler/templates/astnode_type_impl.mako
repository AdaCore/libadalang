## vim: filetype=makocpp

long ${cls.name().lower}_counter = 0;

${cls.name()}::~${cls.name()}() {
        /* In C++, the mother class' destructor is automatically called at the
           end of this function, so iterate only on own fields.  */
        % for t, f in cls_field_decls:
            % if t.is_ptr:
                if (${f.code_name}) ${f.code_name}->dec_ref();
            % endif
        % endfor
#if DEBUG_MODE
        printf("%s", indent_str.c_str());
        printf("DELETING NODE with type ${cls.name()}\n");
#endif
}

std::string ${cls.name()}::repr() {
    std::string result = this->kind_name() + "[" + get_sloc_range().repr() + "]" + "(";

    % for i, (t, f) in enumerate(d for d in all_field_decls if d[1].repr):
        % if i > 0:
            result.append(", ");
        % endif

        % if t.is_ptr:
            if (${f.code_name} != ${t.nullexpr()}) {
        % endif

        result.append(get_repr(${f.code_name}));

        % if t.is_ptr:
            } else result.append("None");
        % endif
    % endfor

    result.append(")");
    return result;
}

## Abstract nodes are never instanciated directly.
% if cls.is_ptr and not cls.abstract:
    static inline ${cls.name()}* ${cls.name()}_new() {
        ${cls.name().lower}_counter++;
        return new ${cls.name()};
    }
% endif

% if not cls.is_ptr:
    ${cls.name()} nil_${cls.name().lower};
% endif

% if not cls.abstract:
    std::string ${cls.name()}::kind_name() { return "${cls.repr_name()}"; }

    void ${cls.name()}::compute_indent_level() {
        % for i, (field_type, field) in enumerate(all_field_decls):
            % if is_ast_node(field_type):
                if (${field.code_name}) {
                    % if field.indent.kind == field.indent.KIND_REL_POS:
                        ${field.code_name}->indent_level = this->indent_level + ${field.indent.rel_pos};
                    % elif field.indent.kind == field.indent.KIND_TOKEN_POS:
                        ${field.code_name}->indent_level = this->${field.indent.token_field_name}.sloc_range.end_column - 1;
                    % endif

                    ${field.code_name}->compute_indent_level();
                }
            % endif
        % endfor
    }

    unsigned ${cls.name()}::get_child_count() const {
        return ${len(astnode_field_decls)};
    }

    bool ${cls.name()}::get_child(unsigned index, ASTNode*& result) {
        ## Some ASTnodes have no ASTNode child: avoid the "unused parameter"
        ## compilation warning for them.
        % if not astnode_field_decls:
            (void) result;
        % endif
        switch (index) {
            % for i, field in enumerate(astnode_field_decls):
                case ${i}:
                    result = ${field.code_name};
                    return true;
            % endfor
            default:
                return false;
        }
    }

    ASTNode *${cls.name()}::lookup_children(const SourceLocation &sloc, bool snap)
    {
        assert(get_sloc_range(snap).compare(sloc) == IN);

        /* Look for a child node that contains SLOC (i.e. return the most
           precise result).  */
        % for i, (field_type, field) in enumerate(all_field_decls):
            % if is_ast_node(field_type):
                /* Note that we assume here that child nodes are ordered so
                   that the first one has a sloc range that is before the sloc
                   range of the second child node, etc.  */
                if (${field.code_name} != ${field_type.nullexpr()}) {
                    auto sub_lookup = ${field.code_name}->lookup_relative(sloc, snap);
                    switch (sub_lookup.first) {
                    case BEFORE:
                        /* If this is the first node, SLOC is before it, so we
                           can stop here.  Otherwise, SLOC is between the
                           previous child node and the next one...  so we can
                           stop here, too.  */
                        return this;

                    case IN:
                        return sub_lookup.second;

                    case AFTER:
                        /* SLOC is after the current child node, so see with
                           the next one.  */
                        break;
                    }
                }
            % endif
        % endfor

        return this;
    }

    void ${cls.name()}::validate() {
       % for t, f in all_field_decls:
         % if is_ast_node (t):
              if (${f.code_name}) {
                 assert(${f.code_name}->parent() == this &&
                        "validate: wrong parent in ${cls.name()}::${f.code_name}");
                 ${f.code_name}->validate();
              }
         % endif
       % endfor
    }

    ptree ${cls.name()}::get_property_tree() {

        ptree result;
        result.put("kind", this->kind_name());
        result.add_child("sloc", get_sloc_range().get_property_tree());

       % for i, (t, f) in enumerate(d for d in all_field_decls if d[1].repr):
           % if t.is_ptr:
               if (${f.code_name} && !${f.code_name}->is_empty_list()) {
                  result.add_child("${f.code_name}", ${f.code_name}->get_property_tree());
               }
           % else:
               result.add_child("${f.code_name}", get_ptree(${f.code_name}));
           % endif
       % endfor

       return result;
    }

    void ${cls.name()}::print_node(int level) {
        print_tab(level);
        std::string result = this->kind_name() + "[" + get_sloc_range().repr() + "]";
        printf("%s\n", result.c_str());

       % for i, (t, f) in enumerate(d for d in all_field_decls if d[1].repr):
           % if t.is_ptr:
               if (${f.code_name} && !${f.code_name}->is_empty_list()) {
                  print_tab(level+1);
                  printf("${f.code_name}:\n");
                  ${f.code_name}->print_node(level+2);
               }
           % else:
               print_tab(level+1);
               printf("${f.code_name}: %s\n", get_repr(${f.code_name}).c_str());
           % endif
       % endfor
    }
% endif
