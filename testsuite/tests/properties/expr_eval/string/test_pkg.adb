with Pkg;

procedure Test_Pkg is
   Index : Character := Pkg.Str (1);
   --% node.f_default_expr.p_eval_as_int

   Slice : String := Pkg.Str (2 .. 3);
   --% node.f_default_expr.p_eval_as_string
begin
   null;
end Test_Pkg;
