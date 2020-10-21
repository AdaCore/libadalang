procedure Test is
   generic
   package Pkg is
      type U is null record;
      pragma Pack (U);
   end Pkg;

   package Pkg_I is new Pkg;

   subtype R_D is Pkg_I.U;
begin
   null;
end Test;
