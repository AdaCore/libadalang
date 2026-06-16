procedure Test is
   type Unconstrained_Arr is array (Positive range <>) of Integer;
   type Constrained_Arr is array (1 .. 128) of Integer;

   U_OK : Unconstrained_Arr (1 .. 128) := (others => 0);
   U_NOK : Unconstrained_Arr := (1 .. 128 => 0);
   C_OK : Constrained_Arr := (others => 0);

   A : constant := U_OK'Length;
   --% node.f_expr.p_is_static_expr()
   --% node.f_expr.p_eval_as_int

   B : Integer := U_NOK'Length;
   --% node.f_default_expr.p_is_static_expr()

   C : constant := C_OK'Length;
   --% node.f_expr.p_is_static_expr()
   --% node.f_expr.p_eval_as_int

   D : constant := Constrained_Arr'Length;
   --% node.f_expr.p_is_static_expr()
   --% node.f_expr.p_eval_as_int
begin
   null;
end Test;
