% if _self.empty_valid:
${pos} := ${pos_name};
% else:
${pos} := -1;
% endif

${res} := new ${_self.parser.get_type_string()}_Vectors.Vector;
${cpos} := ${pos_name};
loop
${pcode}
   exit when ${ppos} = -1;
   ${pos} := ${ppos};
   ${cpos} := ${ppos};
   ${res}.Append (${pres});

   % if _self.sep:
   if Get (Lex, ${cpos}).Q.Id = ${sep_id} then
      ${cpos} := ${cpos} + 1;
   else
      exit;
   end if;
   % endif

end loop;
