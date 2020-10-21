procedure Testop is
   function "+" (A, B : Integer) return Boolean is (True);
   function "+" (A, B : Integer) return Integer is (12);
   function "abs" (A : Integer) return Integer is (12);

   A, B, C, D, E, F : Integer;
   Foo : Boolean;
begin
   A := B + C;
   pragma Test_Statement;

   Foo := B < C;
   pragma Test_Statement;
end Testop;
