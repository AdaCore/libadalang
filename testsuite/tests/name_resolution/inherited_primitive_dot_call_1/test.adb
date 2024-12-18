procedure Test is
   package Pkg is
      type T is tagged null record;
      function Child (X : T'Class; I : Integer) return T is (null record);

      type D is new Pkg.T with null record;
   end Pkg;

   X : Pkg.D;
   Y : Pkg.T;
begin
   X := X.Child (2);
   pragma Test_Statement;
   Y := X.Child (2);
   pragma Test_Statement;
end Test;
