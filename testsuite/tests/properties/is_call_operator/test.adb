procedure Test is
   type T is null record;

   function "+" (A, B : T) return T is (X);
   function "-" (A : T) return T is (A);

   X : T;
   Y : Integer;
begin
   X := X + X;
   --% node.f_expr.f_op.p_is_call
   X := -X;
   --% node.f_expr.f_op.p_is_call
   Y := 40 + 2;
   --% node.f_expr.f_op.p_is_call
end Test;
