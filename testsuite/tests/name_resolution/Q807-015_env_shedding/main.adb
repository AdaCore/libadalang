with Foo;

procedure Main is
   package Foo_Inst is new Foo (Natural, 2);
begin
   Foo_Inst.Bar_Gen_Inst.Count := 3;
   pragma Test_Statement;
end Main;
