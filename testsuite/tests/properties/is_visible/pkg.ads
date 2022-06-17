package Pkg is
   pragma Elaborate_Body;

   Test_Pkg_Public : Integer;

   generic
   package G is
      Test_Pkg_Generic_Child : Integer;
   end G;
private
   Test_Pkg_Private : Integer;

   package Inner is
      Test_Pkg_Inner_Public : Integer;
   private
      Test_Pkg_Inner_Private : Integer;
   end Inner;
end Pkg;
