procedure Enum is
   type My_Enum is (A, B, C, D, E);

   Min : constant := My_Enum'Pos (My_Enum'First);
   --% node.f_expr.p_is_static_expr()

   Max : constant := My_Enum'Pos (My_Enum'Last);
   --% node.f_expr.p_is_static_expr()

   type R1 is range Min .. Max;
   --% range = node.f_type_def.f_range.f_range
   --% range.f_left.p_is_static_expr()
   --% range.f_right.p_is_static_expr()
   --% node.p_is_static_decl()

   type R2 is range My_Enum'Pos (My_Enum'First) .. My_Enum'Pos (My_Enum'Last);
   --% range = node.f_type_def.f_range.f_range
   --% range.f_left.p_is_static_expr()
   --% range.f_right.p_is_static_expr()
   --% node.p_is_static_decl()
begin
   null;
end Enum;
