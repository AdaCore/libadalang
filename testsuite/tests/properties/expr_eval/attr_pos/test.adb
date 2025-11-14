procedure Test is
   type My_Enum is (Enum1, Enum2, Enum3);

   for My_Enum use (Enum1 => 2, Enum2 => 4, Enum3 => 6);

   type Int is range -10 .. 10;

   E : constant := My_Enum'Pos (Enum3);
   --% node.f_expr.p_eval_as_int

   I : constant := Integer'Pos (123);
   --% node.f_expr.p_eval_as_int

   J : constant := Int'Pos (-3);
   --% node.f_expr.p_eval_as_int
begin
   null;
end Test;
