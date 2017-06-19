with Tuple;

procedure Main is
   package My_Tuple is new Tuple (Integer, Float);

   M : My_Tuple.Tuple;

   function Foo return Integer is (12);
   function Foo return Float is (15.0);
begin
   M := (12, 15.0);
   pragma Test_Statement;

   M := (Foo, Foo);
   pragma Test_Statement;
end Main;
