procedure Test is
   package Pkg is
      type T is tagged null record;

      type I is range 0 .. 10;

      procedure Foo (X : T) is null;

      function Bar return T is (null record);

      function Baz return T;

      function Bazz return Integer;
   end Pkg;

   package body Pkg is
      function Baz return T is (null record);
      function Bazz return Integer is (0);
   end Pkg;
begin
   null;
end Test;
