## vim: filetype=makoada

procedure Inc_Ref (Node : in out List_${decl_type(element_type)}) is
begin
   Inc_Ref (AST_Node (Node));
end Inc_Ref;

procedure Dec_Ref (Node : in out List_${decl_type(element_type)}) is
begin
   Dec_Ref (AST_Node (Node));
end Dec_Ref;
