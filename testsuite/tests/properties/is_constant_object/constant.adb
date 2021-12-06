procedure P
  (N : Integer;
   M : in Integer;
   O : in out Integer;
   P : out Integer)
   --% [x.p_is_constant_object for x in node.f_params]
is
   C : constant Integer := 0;
   --% node.p_is_constant_object
   D : Integer := 0;
   --% node.p_is_constant_object
begin
   D := N;
   --% node.f_dest.p_referenced_decl().p_is_constant_object
   --% node.f_expr.p_referenced_decl().p_is_constant_object
   O := D;
   --% node.f_dest.p_referenced_decl().p_is_constant_object
   P := M + C + D + O;
   --% node.f_dest.p_referenced_decl().p_is_constant_object
end P;
