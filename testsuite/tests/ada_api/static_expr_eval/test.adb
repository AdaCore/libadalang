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
      Z : Integer := Integer (3);
      E : My_Enum := My_Enum (B);
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

      type Lower_Character is new Character range 'a' .. 'z';

      Lower_C : Lower_Character := 'c';
   begin
      null;
   end;

   --  Subtypes AttributeRef
   declare
      type A is range -100 .. 100;
      subtype B is A range -10 .. 10;
      subtype C is B;

      A_First : A := A'First;
      A_Last  : A := A'Last;

      B_First : B := B'First;
      B_Last  : B := B'Last;

      C_First : C := C'First;
      C_Last  : C := C'Last;
   begin
      null;
   end;

   --  Membership expressions
   declare
      type A is range -10 .. 10;
      subtype B is A range -10 .. 0;
      subtype C is A range 1 .. 10;

      Test_1 : Boolean := -11 in A;
      Test_2 : Boolean := -10 in A;
      Test_3 : Boolean := 5 in A;
      Test_4 : Boolean := 10 in A;
      Test_5 : Boolean := 11 in A;

      Test_Not_1 : Boolean := 4 not in A;
      Test_Not_2 : Boolean := -12 not in A;

      Test_Multi_1 : Boolean := -11 in B | C;
      Test_Multi_2 : Boolean := -3 in B | C;
      Test_Multi_3 : Boolean := 3 in B | C;
      Test_Multi_4 : Boolean := 4 not in B | C;
      Test_Multi_5 : Boolean := 11 not in B | C;
   begin
      null;
   end;

   --  With substitutions
   declare
      function Foo (X : Integer) return Integer is (X + 2);
      function Bar (X, Y : Integer) return Boolean is (X + Y in -10 .. 10);

      Test_1 : Integer := Foo (2);
      Test_2 : Boolean := Bar (2, 3);
      Test_3 : Boolean := Bar (-3, 2);
      Test_4 : Boolean := Bar (6, 5);
   begin
      null;
   end;

   --  String expressions
   declare
      Test_1 : String := "Hello World!";
      Test_2 : constant String := "Hello";
      Test_3 : constant String := "World!";
      Test_4 : String := Test_2 & " " & Test_3;
   begin
      null;
   end;

   --  Relation operators (Integers)
   declare
      Test_1 : Boolean := 3 =  3;
      Test_2 : Boolean := 3 /= 3;
      Test_3 : Boolean := 3 <  3;
      Test_4 : Boolean := 3 <  4;
      Test_5 : Boolean := 3 <= 3;
      Test_6 : Boolean := 3 >  3;
      Test_7 : Boolean := 3 >  2;
      Test_8 : Boolean := 3 >= 3;
   begin
      null;
   end;

   --  Relation operators (Reals)
   declare
      Test_1 : Boolean := 3.3 =  3.3;
      Test_2 : Boolean := 3.3 /= 3.3;
      Test_3 : Boolean := 3.3 <  3.3;
      Test_4 : Boolean := 3.3 <  3.4;
      Test_5 : Boolean := 3.3 <= 3.3;
      Test_6 : Boolean := 3.3 >  3.3;
      Test_7 : Boolean := 3.3 >  3.2;
      Test_8 : Boolean := 3.3 >= 3.3;
   begin
      null;
   end;

   --  Relation operators (Enums)
   declare
      type T is (A, B);

      Test_1 : Boolean := A = A;
      Test_2 : Boolean := A = B;
      Test_3 : Boolean := A /= A;
      Test_4 : Boolean := A /= B;
   begin
      null;
   end;

   --  Relation operators (Strings)
   declare
      Test_1 : Boolean := "a" = "a";
      Test_2 : Boolean := "a" = "b";
      Test_3 : Boolean := "a" /= "a";
      Test_4 : Boolean := "a" /= "b";
   begin
      null;
   end;

   --  Fixed point ranges
   declare
      type T is delta 0.1 digits 8 range 0.0 .. 1.0;
      type U is delta 0.1 digits 2;
      type V is delta 0.1 range -1.5 .. 1.5;

      T_First : T := T'First;
      T_Last  : T := T'Last;

      U_First : U := U'First;
      U_Last  : U := U'Last;

      V_First : V := V'First;
      V_Last  : V := V'Last;
   begin
      null;
   end;

   --  Invalid decimal fixed point defs
   declare
      type T is delta 1 digits 2;
      type U is delta 0.1 digits 2.5;
      type V is delta 0.1 digits -3;
      type W is delta -0.1 digits 3;

      T_Last : T := T'Last;
      U_Last : U := U'Last;
      V_Last : V := V'Last;
      W_Last : W := W'Last;
   begin
      null;
   end;

   -- Test basic mixed real/integer operations
   declare
      Plus_RI  : Float := 3.5 + 2;
      Plus_IR  : Float := 3 + 2.5;
      Minus_RI : Float := 3.5 - 2;
      Minus_IR : Float := 3 - 2.5;
      Times_RI : Float := 3.5 * 2;
      Times_IR : Float := 3 * 2.5;
      Div_RI   : Float := 3.5 / 2;
      Div_IR   : Float := 3 / 2.5;
   begin
      null;
   end;

end Test;
