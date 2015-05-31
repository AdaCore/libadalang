## vim: filetype=makoada

procedure Inc_Ref (Node : in out List_${decl_type(element_type)}) is
begin
   if Node /= null then
      Inc_Ref (AST_Node (Node));
   end if;
end Inc_Ref;

procedure Dec_Ref (Node : in out List_${decl_type(element_type)}) is
begin
   if Node /= null then
      Dec_Ref (AST_Node (Node));
   end if;
end Dec_Ref;
