procedure Test is
   generic
   package Pkg_G is
      type T is range 1 .. 10;
   end Pkg_G;

   package My_Pkg is new Pkg_G;

   X : My_Pkg.T := 2;
   R : String := My_Pkg.T'Image (X);
   pragma Test_Statement;

   I : Integer := Integer (My_Pkg.T'Value (R));
   pragma Test_Statement;

   type Arr_T is array (My_Pkg.T) of Integer;
   A : Arr_T;
   J : Integer := A (My_Pkg.T'Value (R));
   pragma Test_Statement;
begin
   null;
end Test;

