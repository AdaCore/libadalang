procedure Test is
   type T is ('S' , 'Q' , 'P' , 'M' , 'R');
   subtype ST is T range 'P' .. 'R';

   type U is tagged null record;
   type V is new U with null record;
   type W is new V with null record;

   function Foo return V'Class is (V'(null record));
   function Foo return Integer is (2);

   type I is interface;

   B : Boolean;
begin
   B := 'Q' in ST;
   pragma Test_Statement;

   B := Foo in W'Class;
   pragma Test_Statement;

   B := Foo in I'Class;
   pragma Test_Statement;
end Test;

