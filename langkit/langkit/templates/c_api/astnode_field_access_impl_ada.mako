## vim: filetype=makoada

function ${accessor_name}
  (Node    : ${node_type};
   Value_P : ${field.type.c_type(capi).name}_Ptr) return int
is
   N : constant AST_Node := Unwrap (Node);
begin
   if N.all in ${astnode.name()}_Type'Class then
      declare
         Typed_Node : constant ${astnode.name()} := ${astnode.name()} (N);
      begin
          % if is_enum(field.type):
              Value_P.all := ${field.type.c_type(capi).name}
                (${field.type.name()}'Pos (Typed_Node.${field.name}));
          % elif is_bool(field.type):
              Value_P.all := int (Boolean'Pos (Typed_Node.${field.name}));
          % elif is_ast_node(field.type):
              Value_P.all := Wrap (AST_Node (Typed_Node.${field.name}));
          % elif is_token_type(field.type):
              Value_P.all := Wrap (Typed_Node.${field.name}'Access);
          % else:
              Value_P.all := Typed_Node.${field.name};
          % endif
          return 1;
      end;
   else
      return 0;
   end if;
end ${accessor_name};
