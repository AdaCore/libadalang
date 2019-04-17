package Pkg is

   type T is null record;

   function "+" (X, Y : T) return T;

   subtype R is T;

end Pkg;
