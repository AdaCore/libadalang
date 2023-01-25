package Foo is
   package Bar is
      type T is null record;
   end Bar;

   type R1 is null record;
   type R2 is new R1;
   type R3 is new R1;

   package B renames Bar;

   subtype T is B.T;
   type U is null record;

   procedure Test (X : in out T);
   procedure Test (X : in out U);
end Foo;
