with Foo;

procedure Main is
   package Pouet is
      procedure Print_Int is new Foo.Print_T (Integer, Foo.To_String);
   end Pouet;
begin
   Pouet.Print_Int (12);
   pragma Test_Statement;
end Main;
