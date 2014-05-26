long ${cls.name().lower()}_counter = 0;

% if not cls.abstract:
std::string ${cls.name()}::repr() {
    % if not cls.fields:
    return ${cls.__bases__[0].name()}::repr();
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
%else:

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
