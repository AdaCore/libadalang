## vim: filetype=makoada

--  Start row_submatch

## Parse the element
${parser_context.code}

## If the parsing was successful then
if ${parser_context.pos_var_name} /= -1 then

   ## Increment the refcount of parsed element if applicable
   % if is_ast_node (parser.get_type()) and _self.components_need_inc_ref:
      if ${parser_context.res_var_name} /= null then
         Inc_Ref (${parser_context.res_var_name});
      end if;
   % endif

   ## Set current position to the out position of the parsed row element
   ${pos} := ${parser_context.pos_var_name};

   ## Store the result if it is not discarded
   % if not parser.discard():
      ${subresult} := ${parser_context.res_var_name};
   % endif

else
   ## If the parsing was unsuccessful, then set the position accordingly
   ${pos} := -1;

   ## And then go to the appropriate exit label (always exit label 0 if there
   ## is no refcounting)
   goto ${"{}_{}".format(exit_label, i if _self.components_need_inc_ref else 0)};

end if;

--  End row_submatch
