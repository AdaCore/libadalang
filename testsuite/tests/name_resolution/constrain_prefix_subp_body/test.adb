procedure Test is
   package Pkg1 is
      type T is tagged null record;

      function Foo (X : T) return Integer is (1);
   end Pkg1;

   package Pkg2 is
      type T is tagged null record;

      function Foo (X : T) return Integer is (2);
   end Pkg2;

   function Bar (X : Integer) return Pkg1.T is (null record);
   function Bar (Y : Boolean) return Pkg2.T is (null record);

   X : Integer;
begin
   X := Bar (1).Foo;
   pragma Test_Statement;
end Test;
