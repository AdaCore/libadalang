procedure Test is
   type A is tagged null record;

   type B is new A with record
      X : Integer;
   end record;

   function Foo return Boolean is (True);
   function Foo return A is (null record);
   function Foo return Integer is (0);

   Value : B := (Foo with X => 2);
   pragma Test_Statement;
begin
   null;
end Test;
