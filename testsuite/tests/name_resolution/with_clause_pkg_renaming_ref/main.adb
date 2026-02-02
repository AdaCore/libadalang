with Renaming;
with Renaming.Renaming;
with Std;

procedure Main is
begin
   Renaming.Foo;
   pragma Test_Statement;
   Test.Foo;

   Renaming.Renaming.Bar;
   pragma Test_Statement;

   declare
      X : Std.Boolean;
      pragma Test_Statement;
   begin
      null;
   end;
end Main;
