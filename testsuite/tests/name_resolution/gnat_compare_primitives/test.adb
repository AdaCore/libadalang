procedure Test is
   package Pkg is
      type T is tagged null record;

      type I is range 0 .. 10;

      procedure Foo (X : T) is null;

      function Bar return T is (null record);

      function Baz return T;

      function Bazz return Integer;

      procedure Also (X : I; Y : T) is null;

      type Priv is private;
      type Priv_Tagged is tagged private;

      function Foo_Bar (X : I; Y : Priv_Tagged) return Integer;

   private

      type Priv is tagged null record;
      type Priv_Tagged is tagged null record;

      function Foo_Baz (X : I; Y : Priv) return Integer is (0);
   end Pkg;

   package body Pkg is
      function Baz return T is (null record);
      function Bazz return Integer is (0);
      function Foo_Bar (X : I; Y : Priv_Tagged) return Integer is (0);
   end Pkg;
begin
   null;
end Test;
