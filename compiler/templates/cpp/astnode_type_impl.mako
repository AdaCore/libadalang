## vim: filetype=cpp

long ${cls.name().lower()}_counter = 0;

${cls.name()}::~${cls.name()}() {
        /* In C++, the mother class' destructor is automatically called at the
           end of this function, so iterate only on own fields.  */
        % for t, f in cls_field_decls:
            % if t.is_ptr:
                if (${f.name}) ${f.name}->dec_ref();
            % endif
        % endfor
#if DEBUG_MODE
        printf("%s", indent_str.c_str());
        printf("DELETING NODE with type ${cls.name()}\n");
#endif
}

std::string ${cls.name()}::repr() {
    std::string result = this->__name() + "(";

    % for i, (t, f) in enumerate(d for d in all_field_decls if d[1].repr):
        % if i > 0:
            result.append(", ");
        % endif

        % if t.is_ptr:
            if (${f.name} != ${t.nullexpr()}) {
        % endif

            result.append(get_repr(${f.name}));

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
        ${cls.name().lower()}_counter++;
        return new ${cls.name()};
    }
% endif

% if not cls.is_ptr:
    ${cls.name()} nil_${cls.name()};
% endif

std::string ${cls.name()}::__name() { return "${cls.repr_name()}"; }
