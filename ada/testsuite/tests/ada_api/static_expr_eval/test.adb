procedure Test is
   type My_Enum is (A, B, C);

   --  The main will iterate through every object decl, trying to statically
   --  evaluate the default expression.

   Expr_1 : My_Enum := A;
   Expr_2 : My_Enum := B;
   Expr_3 : Integer := 12;
   Expr_4 : Float := 12.0;
   Expr_5 : Integer := Expr_3;
begin
   null;
end Test;
