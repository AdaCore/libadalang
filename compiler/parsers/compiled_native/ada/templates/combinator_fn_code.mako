${fn_profile}
is
   % for d, e in defs:
   ${d} : ${e};
   % endfor
begin
${code}
   Current_Pos := ${pos};
   return ${res};
end ${_self.gen_fn_name};
