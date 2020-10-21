procedure Main is
   package Foo is
      type T is null record;
      pragma Find_All_References (Any, Imprecise_Fallback => True);

      A : T := (null record);
   end Foo;


   package Bar is
      function Baz (A : Integer) return Float;
      pragma Find_All_References (Any, Imprecise_Fallback => True);
   end Bar;

   package body Bar is
      A : Float := Baz (12);
      B : Integer := Baz (12);
      --  This one will resolve to function Baz in imprecise mode
   end Bar;

   A : Float := Lol.Bar.Baz (12, Poo);
   --  Resolution of this statement will fail even in imprecise_fallback mode
   --  because there is no `Baz` visible here. Even so, it shouldn't crash
   --  find_all_references for Baz.
begin
   null;
end Main;
