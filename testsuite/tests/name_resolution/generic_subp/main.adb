with Foo;

procedure Main is
   procedure Print_Int is new Foo.Print_T (Integer, Foo.To_String);
begin
   Print_Int (12);
   pragma Test_Statement;
end Main;
