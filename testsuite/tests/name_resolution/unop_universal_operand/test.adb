procedure Test is
   type Real is digits 6;
   type Mod_Type is mod 256;

   function Sqrt (X : Real) return Real is (X);

   --  In a universal context, a unary arithmetic operator resolves to the
   --  root numeric type (root_integer / root_real), like a binary one.
   Root_Int  : constant := -1;
   pragma Test_Statement;
   Root_Real : constant := -1.0;
   pragma Test_Statement;

   --  Nested and combined unary operators keep resolving to root_real
   Abs_Real  : constant := abs (-3.75);
   pragma Test_Statement;
   Mul_Real  : constant := (-2.5) * (-1.5);
   pragma Test_Statement;

   --  In a typed context, the specific type is used instead
   Typed_Int : Integer := -1;
   pragma Test_Statement;

   R : Real;
   M : Mod_Type;
begin
   R := Sqrt (Root_Real);
   pragma Test_Statement;

   --  A unary operator used directly in a call takes the specific type
   --  expected by the formal (here the user type Real).
   R := Sqrt (-1.0);
   pragma Test_Statement;

   --  "not" on a universal integer literal takes the expected type from
   --  the context (the modular type), not the universal type.
   M := not 170;
   pragma Test_Statement;
end Test;
