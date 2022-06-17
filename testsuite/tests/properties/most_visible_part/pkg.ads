package Pkg is
   pragma Elaborate_Body;

   type Test_Pkg is private;

   generic
   package G is
      type Test_Pkg_Generic_Child is private;
   private
      type Test_Pkg_Generic_Child is null record;
   end G;
private
   type Test_Pkg is null record;

   package Inner is
      type Test_Pkg_Inner is private;
   private
      type Test_Pkg_Inner is null record;
   end Inner;
end Pkg;
