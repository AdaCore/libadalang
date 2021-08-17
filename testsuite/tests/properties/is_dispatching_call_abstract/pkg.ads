package Pkg is
   type T is abstract tagged private;

   procedure Foo (X : access T) is abstract;

private
   type T is abstract tagged null record;
end Pkg;
