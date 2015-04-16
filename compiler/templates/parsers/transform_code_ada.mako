## vim: filetype=makoada

--  Start transform_code

${parser_context.code}
% if _self.get_type().is_ptr:
   ${res} := ${_self.typ.nullexpr()};
% endif

if ${parser_context.pos_var_name} /= -1 then
   % if not is_row(_self.parser) and _self.parser.needs_refcount():
      if ${parser_context.res_var_name} /= null then
         Inc_Ref (${parser_context.res_var_name});
      end if;
   % endif

   % if _self.get_type().is_ptr:
       ${res} := new ${_self.typ.name()}_Type;
   % endif

   ## Compute and set the sloc range for this AST node.
   ${res}.Token_Data := Parser.TDH;
   ${res}.Token_Start := ${pos_name};
   ${res}.Token_End := (if ${parser_context.pos_var_name} = ${pos_name}
                        then ${pos_name}
                        else ${parser_context.pos_var_name} - 1);

   % for f, arg, typ in zip(_self.typ.get_fields(), args, _self.get_type().get_types(_compile_ctx)):
      ${res}.F_${f.name} := ${arg};
      % if is_ast_node (typ):
         if ${arg} /= null then
            ${arg}.Parent := AST_Node (${res});
         end if;
      % endif
   % endfor
end if;

--  End transform_code
