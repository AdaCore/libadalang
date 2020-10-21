procedure Main is
   procedure Foo (X : Integer);
   package Pkg is
      type T is tagged null record;

      procedure Bar (Self : T; X : Integer);
   end Pkg;

   E : Pkg.T;
begin
   Foo (X => 2);
   E.Bar (X => 2);
end Main;
