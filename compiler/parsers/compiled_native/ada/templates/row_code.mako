${pos} := ${pos_name};
${did_fail} := False;
${body}
<<${exit_label}>>

% if _self.make_tuple:
if not ${did_fail} then
   ${res} := ${_self.type_name}'(${row_rec_elems});
end if;
% endif
