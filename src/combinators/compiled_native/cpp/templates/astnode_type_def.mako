class ${cls.name()} : public ${base_name} {
public:
% for m, f in zip(matchers, cls.fields):
     ${f.tstring() + "*" if f.tstring() else m.get_type_string()} ${f.name};
% endfor
% if not cls.abstract:
    std::string repr();
% endif
% if cls.fields:
    ${cls.name()}() : ${", ".join("{0}({1})".format(f.name, m.nullexpr()) for m, f in zip(matchers, cls.fields))} {}
% endif
    std::string __name();
};

extern long ${cls.name().lower()}_counter;

% if not cls.is_ptr:
extern ${cls.name()} nil_${cls.name()};
% endif
