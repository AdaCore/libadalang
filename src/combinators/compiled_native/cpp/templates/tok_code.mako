${res} = get (lex, ${pos_name});
${pos} = ${pos_name} + 1;
if (${res}._id != ${_self._id}) {
   ${pos} = -1;
   ${res} = no_token;
}
