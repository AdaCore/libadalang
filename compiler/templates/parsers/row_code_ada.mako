## vim: filetype=makoada

--  Start row_code

${pos} := ${pos_name};
${did_fail} := False;
${body}

goto ${exit_label}_0;
% for i in range(len(_self.parsers) - 1, -1, -1):
   <<${exit_label}_${i}>>
   % if i > 0 and _self.components_need_inc_ref and _self.parsers[i-1].needs_refcount() and not _self.parsers[i-1].discard():
      % if _self.parsers[i-1].get_type().is_ptr:
         if ${subresults[i-1]} /= null then
            Dec_Ref (${subresults[i-1]});
         end if;
      % endif
   % endif
% endfor

--  End row_code
