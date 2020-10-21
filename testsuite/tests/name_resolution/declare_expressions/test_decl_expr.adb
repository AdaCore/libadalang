procedure Test_Decl_Expr is
   A : Integer := (declare B : Integer := 12; begin B);
   pragma Test_Statement;
begin
   null;
end Test_Decl_Expr;
