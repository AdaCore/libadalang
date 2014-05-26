${code}
${res} := ${_self.typ.nullexpr()};
if ${cpos} /= -1 then
   ${res} := ${_self.typ.create_instantiation(args)};
end if;
