procedure Test is
   generic
      type T is range <>;
   package Pkg is
      X : T'Base := T'First;
   end Pkg;

   package My_Pkg is new Pkg (Positive);
   pragma Test_Statement;
begin
   My_Pkg.X := 2;
end Test;
