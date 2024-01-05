procedure Test_Access is
   package Pkg is
      type T is null record;
      type T_Access is access all T;

      procedure Foo (Self : T_Access) is null;
   end Pkg;

   use Pkg;

   X : T_Access;
begin
   X.Foo;
   pragma Test_Statement;
end Test_Access;
