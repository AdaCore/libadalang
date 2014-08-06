## vim: filetype=cpp

long ${cls.name().lower()}_counter = 0;

${cls.name()}::~${cls.name()}() {
        % for m, f in zip(matchers, cls.fields):
            % if m.is_ptr():
                if (${f.name}) ${f.name}->dec_ref();
            % endif
        % endfor
#if DEBUG_MODE
        printf("%s", indent_str.c_str());
        printf("DELETING NODE with type ${cls.name()}\n");
#endif
}


% if not cls.abstract:
    std::string ${cls.name()}::repr() {
        % if not cls.fields:
            % if cls.__bases__[0].fields:
                return ${cls.__bases__[0].name()}::repr();
            % else:
                return this->__name() + "()";
            % endif
        % else:
            std::string result = this->__name() + "(";

            % for i, (m, f) in enumerate(repr_m_to_fields):
                % if f.opt:
                    if (${f.name} != ${m.nullexpr()}) {
                % endif

                result.append(${m.emit_repr("this->" + f.name)});

                % if f.opt:
                    } else result.append("None");
                % endif

                % if i < len(repr_m_to_fields) - 1:
                    result.append(", ");
                % endif
            % endfor

            result.append(")");
            return result;
        % endif
    }
% endif

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
