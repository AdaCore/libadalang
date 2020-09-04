procedure Test is
   type Integer_Access is access all Integer;

   package Pkg is
      type T is tagged record
         Foo : Integer_Access;
      end record;

      procedure Foo (Self : T);
   end Pkg;

   package body Pkg is
      procedure Foo (Self : T) is null;
   end Pkg;

   X : Pkg.T;
begin
   if X.Foo /= null then
      null;
   end if;
   pragma Test_Statement;
end Test;
