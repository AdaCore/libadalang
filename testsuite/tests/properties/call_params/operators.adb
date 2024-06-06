procedure Operators is
   type T is range -100 .. 100;

   X, Y : T := 1;
begin
   X := "+" (Left => X, Right => Y);
   --% node.f_expr.p_call_params

   Y := "+" (Right => X, Left => Y);
   --% node.f_expr.p_call_params

   X := "-" (Right => Y);
   --% node.f_expr.p_call_params
end Operators;
