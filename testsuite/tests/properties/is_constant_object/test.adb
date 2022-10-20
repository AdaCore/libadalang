package Test is
   procedure B is
      C    : constant Integer := 0;
      D, E : Integer := 0;
   begin
      D := Test.B.C;
      --% node.f_dest.p_is_constant
      --% node.f_expr.p_is_constant
      E := Test.B.D;
      --% node.f_dest.p_is_constant
      --% node.f_expr.p_is_constant
   end B;
end Test;
