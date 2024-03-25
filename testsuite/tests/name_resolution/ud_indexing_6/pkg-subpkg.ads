package Pkg.Subpkg is
   type New_Window is new Pkg.Window with private;

   overriding
   function CRef (W : in New_Window; N : in Name) return Pixel;

   not overriding
   function CRef (W : in New_Window; N : in Name) return Integer;
private
   type New_Window is new Pkg.Window with null record;
end Pkg.Subpkg;
