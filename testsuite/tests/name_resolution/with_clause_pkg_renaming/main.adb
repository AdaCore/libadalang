with Renaming, Renaming.Child;
pragma Test_Statement;

procedure Main is
begin
   Renaming.Foo;
   pragma Test_Statement;
   Test.Foo;
   pragma Test_Statement;

   Renaming.Child.Bar;
   pragma Test_Statement;
   Test.Child.Bar;
   pragma Test_Statement;
end Main;
