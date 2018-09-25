with Gen;

procedure Main is
   package Gen_Inst is new Gen;
   package Nested_Inst is new Gen_Inst.Nested;

   A : Integer;
   B : Nested_Inst.Poo;
   C : Gen_Inst.Rec;
begin
   A := Nested_Inst.Foo;
   B := Nested_Inst.Bar;
   C := Nested_Inst.Baz;
end Main;
