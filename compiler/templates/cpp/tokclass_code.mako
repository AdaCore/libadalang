${res} = get (lex, ${pos_name});
${pos} = ${pos_name} + 1;
if (${res}.id != ${id}) {
   ${pos} = -1;
   ${res} = no_token;
}
