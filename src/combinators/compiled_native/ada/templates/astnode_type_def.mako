type ${cls.name()}_Record is new ${base_name}_Record with record
% for m, f in zip(matchers, cls.fields):
   ${f.name} : ${m.get_type_string()};
% endfor
% if len(cls.fields) == 0:
   null;
% endif
end record;
