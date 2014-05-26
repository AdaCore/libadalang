${res} := Get (Lex, ${pos_name});
${pos} := ${pos_name} + 1;
if ${res}.Q.Id /= ${_self._id} then
   ${pos} := -1;
   ${res} := NoToken;
end if;
