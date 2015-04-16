## vim: filetype=makoada

--  Start list_code

% if _self.empty_valid:
    ${pos} := ${pos_name};
% else:
    ${pos} := -1;
% endif

${res} := ${_self.get_type().nullexpr()};

${cpos} := ${pos_name};

loop
   ${parser_context.code}
   exit when ${parser_context.pos_var_name} = -1;

   ${pos} := ${parser_context.pos_var_name};
   % if cpos != parser_context.pos_var_name:
      ${cpos} := ${parser_context.pos_var_name};
   % endif

   % if _self.revtree_class:
      if ${res} = ${_self.get_type().nullexpr()} then
         ${res} := ${_self.get_type().name()} (${parser_context.res_var_name});
      else
         declare
            New_Res : ${_self.revtree_class.name()} :=
               new ${_self.revtree_class.name()}_Type;
         begin
            New_Res.F_${_self.revtree_class.fields[0].name} := ${res};
            New_Res.F_${_self.revtree_class.fields[1].name} :=
               ${_self.get_type().name()} (${parser_context.res_var_name});
            Inc_Ref (${res});
            Inc_Ref (${parser_context.res_var_name});
            ${res}.Parent := AST_Node (New_Res);
            ${parser_context.res_var_name}.Parent := AST_Node (New_Res);
            ${res} := ${_self.get_type().name()} (New_Res);
         end;
         ${res}.Token_Data := Parser.TDH;
         ${res}.Token_Start := ${pos_name};
         ${res}.Token_End := (if ${cpos} = ${pos_name}
                              then ${pos_name}
                              else ${cpos} - 1);
      end if;

   % else:
      if ${res} = ${_self.get_type().nullexpr()} then
         ${res} := new List_${decl_type(_self.parser.get_type())}_Type;
      end if;
      ${res}.Vec.Append (${parser_context.res_var_name});
      % if is_ast_node (_self.parser.get_type()):
         if ${parser_context.res_var_name} /= null then
            ${parser_context.res_var_name}.Parent := AST_Node (${res});
         end if;
      % endif

      % if _self.parser.needs_refcount():
         % if _self.parser.get_type().is_ptr:
            if ${parser_context.res_var_name} /= null then
               Inc_Ref (${parser_context.res_var_name});
            end if;
         % else:
            Inc_Ref (${parser_context.res_var_name});
        % endif
      % endif

   % endif

   % if _self.sep:
      ${sep_context.code}
      if ${sep_context.pos_var_name} /= -1 then
          ${cpos} := ${sep_context.pos_var_name};
      else
         exit;
      end if;
   % endif
end loop;

## If we managed to parse a list, compute and set the sloc range for this AST
## node.
if ${res} /= null then
   ${res}.Token_Data := Parser.TDH;
   ${res}.Token_Start := ${pos_name};
   ${res}.Token_End := (if ${cpos} = ${pos_name}
                        then ${pos_name}
                        else ${cpos} - 1);
end if;

--  End list_code
