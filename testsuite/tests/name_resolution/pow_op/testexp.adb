procedure Testexp is
   A : Float := 12412335125.99 ** 2;
   pragma Test_Statement;

   type My_Int is range -1000 .. 1000;

   function Ident_Int (X : Integer) return Integer is (X);
   --  Used in expressions below to make them non-static and avoid GNAT
   --  from rejected programs that it can prove would raise Constraint_Errors.

   B : Boolean := My_Int'(3) ** Integer'(4) <= 100;
   pragma Test_Statement;

   C : Boolean := Float'(2.0) ** Integer'(3) /= 8.0;
   pragma Test_Statement;

   D : Float := Float'Base'Last ** Ident_Int (2);
   pragma Test_Statement;
begin
   null;
end Testexp;
