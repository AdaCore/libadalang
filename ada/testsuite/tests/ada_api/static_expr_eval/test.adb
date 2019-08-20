procedure Test is
   type My_Enum is (A, B, C);

   --  The main will iterate through every object decl, trying to statically
   --  evaluate the default expression.

   Expr_1 : My_Enum := A;
   Expr_2 : My_Enum := B;
   Expr_3 : Integer := 12;
   Expr_4 : Float := 12.0;
   Expr_5 : Integer := Expr_3;

   Tmp : Integer;
   Expr_6 : Integer := Tmp;
begin
   declare
      -- Test basic operation on reals
      X : Float := 3.5;
      Y : Float := 1.5;

      Plus  : Float := X + Y;
      Minus : Float := X - Y;
      Times : Float := X * Y;
      Div   : Float := X / Y;
   begin
      null;
   end;

   declare
      -- Test basic operations on integers
      X : Integer := 3;
      Y : Integer := 2;

      Plus  : Integer := X + Y;
      Minus : Integer := X - Y;
      Times : Integer := X * Y;
      Div   : Integer := X / Y;
   begin
      null;
   end;

   --  Type conversions and attributes
   declare
      X : Integer := Integer (3.0 * 12.4);
      Y : Integer := Integer'Max (12, 14);
      Z : Integer := Float'Max (12.0, 14.5);
      Z : Integer := Float'Min (12.0, 14.5);
      Z : Integer := Integer'Min (X, Y);
   begin
      null;
   end;

   --  Characters
   declare
      A : Character := 'a';
      C : Character := 'c';

      Wide_A     : Wide_Character := 'a';
      Wide_Gamma : Wide_Character := 'γ';

      Wide_Wide_A      : Wide_Wide_Character := 'a';
      Wide_Wide_Smiley : Wide_Wide_Character := '😀';
   begin
      null;
   end;
end Test;
