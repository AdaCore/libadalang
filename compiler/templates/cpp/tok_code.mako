${res} = get (this->lexer, ${pos_name});
${pos} = ${pos_name} + 1;
if (${res}.id != ${token_kind}) {
   ${pos} = -1;
   ${res} = no_token;
}
