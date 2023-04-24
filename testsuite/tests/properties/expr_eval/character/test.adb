procedure Test is
   NUL : Character := ASCII.NUL;
   --% node.f_default_expr.p_eval_as_int
   SOH : Character := ASCII.SOH;
   --% node.f_default_expr.p_eval_as_int
   US : Character := ASCII.US;
   --% node.f_default_expr.p_eval_as_int
   DEL : Character := ASCII.DEL;
   --% node.f_default_expr.p_eval_as_int

   DEL_V : Character := Character'Val(16#7F#);
   --% node.f_default_expr.p_eval_as_int
   Hex_0000BEEF : Wide_Character := Wide_Character'Val(16#0000BEEF#);
   --% node.f_default_expr.p_eval_as_int
   Hex_1EEEBEEF : Wide_Wide_Character := Wide_Wide_Character'Val(16#1EEEBEEF#);
   --% node.f_default_expr.p_eval_as_int
   Hex_7FFFFFFE : Wide_Wide_Character := Wide_Wide_Character'Val(16#7FFFFFFE#);
   --% node.f_default_expr.p_eval_as_int
   Hex_7FFFFFFF : Wide_Wide_Character := Wide_Wide_Character'Val(16#7FFFFFFF#);
   --% node.f_default_expr.p_eval_as_int
   Hex_7FFFFFFF : Wide_Wide_Character := Wide_Wide_Character'Succ(Hex_7FFFFFFE);
   --% node.f_default_expr.p_eval_as_int

   First : Character := Character'First;
   --% node.f_default_expr.p_eval_as_int
   W_First : Wide_Character := Wide_Character'First;
   --% node.f_default_expr.p_eval_as_int
   W_W_First : Wide_Wide_Character := Wide_Wide_Character'First;
   --% node.f_default_expr.p_eval_as_int
   Last : Character := Character'Last;
   --% node.f_default_expr.p_eval_as_int
   W_Last : Wide_Character := Wide_Character'Last;
   --% node.f_default_expr.p_eval_as_int
   W_W_Last : Wide_Wide_Character := Wide_Wide_Character'Last;
   --% node.f_default_expr.p_eval_as_int

   Hex_80000000 : Wide_Wide_Character := Wide_Wide_Character'Succ(Hex_7FFFFFFF);
   -- Invalid, wrong result expected
   --% node.f_default_expr.p_eval_as_int

   B : Boolean := Hex_7FFFFFFE < Hex_7FFFFFFF;
   --% node.f_default_expr.p_eval_as_int
begin
   null;
end Test;
