procedure Test is
   type T is range 1 .. 5;

   X : T := 1;
begin
   X := "+" (X, X);
   --% node.f_expr.p_call_params
end Test;
