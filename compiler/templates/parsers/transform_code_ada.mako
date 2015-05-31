## vim: filetype=makoada

--  Start transform_code

${parser_context.code}

if ${parser_context.pos_var_name} /= -1 then

   ## Increment ref count of parsed result if needed (eg. if the transform is
   ## just a simple wrapper around another AST Node)
   % if is_ast_node (_self.parser.get_type()):
      if ${parser_context.res_var_name} /= null then
         Inc_Ref (${parser_context.res_var_name});
      end if;
   % endif

   ## Create the transform wrapper node
   ${res} := new ${_self.typ.name()}_Type;

   ## Compute and set the sloc range for this AST node.
   ${res}.Token_Data := Parser.TDH;
   ${res}.Token_Start := ${pos_name};
   ${res}.Token_End := (if ${parser_context.pos_var_name} = ${pos_name}
                        then ${pos_name}
                        else ${parser_context.pos_var_name} - 1);

   % for f, arg, typ in zip(_self.typ.get_fields(), args, _self.get_type().get_types(_compile_ctx)):
      ## Set children fields into the created node
      ${res}.F_${f.name} := ${arg};

      ## Set the parent backlink if needed
      % if is_ast_node (typ):
         if ${arg} /= null then
            ${arg}.Parent := AST_Node (${res});
         end if;
      % endif
   % endfor

end if;

--  End transform_code
