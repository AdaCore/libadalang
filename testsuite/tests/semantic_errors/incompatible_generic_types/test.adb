procedure Test is
   generic
   package Pkg is
      type T is null record;
   end Pkg;

   package Pkg_1 is new Pkg;
   package Pkg_2 is new Pkg;

   X_1 : Pkg_1.T;
   X_2 : Pkg_2.T;
begin
   X_1 := X_2;
   pragma Test_Statement (Expect_Fail => True);
end Test;
