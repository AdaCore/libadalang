package body Pkg.Subpkg is
   function CRef (W : in New_Window; N : in Name) return Pixel is
   begin
      return W.Pixels (2);
   end Cref;

   function CRef (W : in New_Window; N : in Name) return Integer is
   begin
      return W.Count;
   end Cref;
end Pkg.Subpkg;
