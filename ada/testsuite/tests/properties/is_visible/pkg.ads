package Pkg is
   Test_Pkg_Public : Integer;
private
   Test_Pkg_Private : Integer;

   package Inner is
      Test_Pkg_Inner_Public : Integer;
   private
      Test_Pkg_Inner_Private : Integer;
   end Inner;
end Pkg;
