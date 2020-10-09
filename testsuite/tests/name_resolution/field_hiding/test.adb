procedure Test is
   package Pkg is
      type T is tagged record
         Foo : Integer;
      end record;

      function Foo (Self : T) return Integer;
      function Bar (Self : T) return Integer is (42);
   end Pkg;

   package body Pkg is
      function Foo (Self : T) return Integer is
      begin
         return Self.Foo;
         pragma Test_Statement;
      end Foo;
   end Pkg;

   X : Pkg.T := (Foo => 2);
   Y : Integer := X.Foo;
   pragma Test_Statement;

   Z : Integer := X.Bar;
   pragma Test_Statement;
begin
   null;
end Test;
