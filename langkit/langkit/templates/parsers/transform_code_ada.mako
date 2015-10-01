## vim: filetype=makoada

--  Start transform_code

${parser_context.code}

if ${parser_context.pos_var_name} /= -1 then

   ## Create the transform wrapper node
   ${res} := ${_self.typ.name()}
     (${_self.typ.name()}_Alloc.Alloc (Parser.Mem_Pool));

   ## Compute and set the sloc range for this AST node.
   ${res}.Token_Data := Parser.TDH;
   ${res}.Token_Start := ${pos_name};
   ${res}.Token_End := (if ${parser_context.pos_var_name} = ${pos_name}
                        then ${pos_name}
                        else ${parser_context.pos_var_name} - 1);

   % for field, arg in zip(_self.typ.get_fields(), args):
      ## Set children fields into the created node
      ${res}.${field.name} :=
         % if is_ast_node(field.type):
            ${decl_type(field.type)} (${arg});
         % else:
            ${arg};
         % endif

      ## Set the parent backlink if needed
      % if is_ast_node(field.type):
         if ${arg} /= null then
            ${arg}.Parent := AST_Node (${res});
         end if;
      % endif
   % endfor

end if;

--  End transform_code
