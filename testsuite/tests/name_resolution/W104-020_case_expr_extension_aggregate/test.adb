procedure Test is
   type A is tagged null record;

   type B is new A with record
      X : Integer;
   end record;

   function Foo return A is (null record);

   Value : Natural;
begin
   Value := (case B'(Foo with X => 2).X is
             when others => 42);
   pragma Test_Statement;
end Test;

