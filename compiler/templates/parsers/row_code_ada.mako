## vim: filetype=makoada

--  Start row_code

${pos} := ${pos_name};

## This is the main body of the row, which is the concatenation of the code for
## each row part
${body}

## Don't emit cleanup code at all if components are not ref counted
% if _self.components_need_inc_ref:

   ## If we arrive here, everything went fine, so we just want to jump over the
   ## clean up code and go straight to the next part
   goto ${exit_label}_0;

   ## This is the cleanup code. Each sub-row has a label, and the labels are
   ## organized in reverse parsing order, so that if you have the following row:
   ## Row (A, B, C, D, E, F)
   ##            ^ Fail here
   ## We will only execute the cleanup code for A and B

   % for i in range(len(_self.parsers) - 1, 0, -1):
      <<${exit_label}_${i}>>
      % if is_ast_node(_self.parsers[i-1].get_type()):
         Dec_Ref (${subresults[i-1]});
      % endif
   % endfor

% endif

## This is the label for "no cleanup code"
<<${exit_label}_0>>


--  End row_code
