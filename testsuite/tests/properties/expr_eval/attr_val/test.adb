with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Enum is (A, B, C);

   type Int is range -10 .. 10;

   R : constant Enum := Enum'Val (0);
   --% node.f_default_expr.p_is_static_expr()
   --% node.f_default_expr.p_eval_as_int

   S : constant Enum := Enum'Val (3);
   --% node.f_default_expr.p_eval_as_int

   T : constant Enum := Enum'Val (-1);
   --% node.f_default_expr.p_eval_as_int

   U : constant Int := Int'Val (-3);
   --% node.f_default_expr.p_eval_as_int
begin
   Put_Line (R'Image);
end Test;
