procedure Test is
   type T1 is null record;
   type T2 is null record;
   type T3 is null record;
   type T4 is null record;
   type T5 is null record;
   type T6 is null record;
   type T7 is null record;
   type T8 is null record;
   type T9 is null record;

   X : Integer;

   function Foo (X : T1) return Integer is (0);
   function Foo (X : T2) return Integer is (0);
   function Foo (X : T3) return Integer is (0);
   function Foo (X : T4) return Integer is (0);
   function Foo (X : T5) return Integer is (0);
   function Foo (X : T6) return Integer is (0);
   function Foo (X : T7) return Integer is (0);
   function Foo (X : T8) return Integer is (0);
   function Foo (X : T9) return Integer is (0);
begin
   X := Foo (0);
   pragma Test_Statement;
end Test;
