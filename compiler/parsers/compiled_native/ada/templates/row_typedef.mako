type ${_self.type_name} is record
% for i, m in enumerate(_self.parsers):
   Field_${i} : ${m.get_type_string()};
% endfor
end record;
