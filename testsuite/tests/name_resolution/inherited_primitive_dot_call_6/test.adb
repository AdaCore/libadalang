procedure Test is
   package Pkg is
      type T is tagged null record;
      function Child (X: T'Class; I : Integer) return T is (null record);

      type D is new T with null record;
   end Pkg;

   package Other is
      type E is new Pkg.T with null record;
   end Other;

   use Pkg;
   use Other;

   X : D;
   Y : E;
begin
   X := X.Child (2);
   pragma Test_Statement;
   Y := Y.Child (2);
   pragma Test_Statement;
end Test;
