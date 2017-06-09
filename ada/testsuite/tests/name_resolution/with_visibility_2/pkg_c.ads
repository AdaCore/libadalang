with Pkg_A.Pkg_B;

package Pkg_C is
   A_I : Integer renames Pkg_A.I;
   B_I : Integer renames Pkg_A.Pkg_B.I;

   pragma Test (Pkg_A.I);
   pragma Test (Pkg_A.Pkg_B.I);
end Pkg_C;
