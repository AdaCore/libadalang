package Valid is

   type Int is range 1 .. 1000;

   I0 : constant := 1;

   I1 : constant Integer := Integer (I0);
   --% node.f_default_expr.p_eval_as_int

   I2 : constant Int := Int (I1);
   --% node.f_default_expr.p_eval_as_int

   type Enum is (A, B, C);
   type New_Enum is new Enum;

   E1 : constant Enum := A;
   --% node.f_default_expr.p_eval_as_int

   E2 : constant New_Enum := New_Enum (E1);
   --% node.f_default_expr.p_eval_as_int

end Valid;
