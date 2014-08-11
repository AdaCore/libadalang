if ${mpos} /= -1 then
   ${pos} := ${mpos};
% if not is_discard:
   ${subresult} := ${mres};
% endif
else
   ${pos} := -1;
   ${did_fail} := True;
   goto ${exit_label};
end if;
