package Pkg.Child_Pkg is
   pragma Elaborate_Body;

   type Origin_Pkg_Child_Pkg_Public is private;
private
   type Origin_Pkg_Child_Pkg_Private is null record;
end Pkg.Child_Pkg;
