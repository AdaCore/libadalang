procedure Testop is
   type My_Int is range 1 .. 1205497;

   function "+" (A, B : Integer) return Boolean is (True);
   function "+" (A, B : Integer) return Integer is (12);
   function "abs" (A : Integer) return Integer is (12);

   A, B, C, D, E, F : Integer;

   O, P, Q : My_Int;

   Foo : Boolean;
begin
   if A < B and then Foo or else ((A + B) < C) then
      null;
   end if;
   pragma Test_Statement;
end Testop;
