package Pkg is
   type T is private;

   function Foo return T;

private
   type T is null record;
end Pkg;
