with Data_Pkg;

package D_Data is
   package Pkg_H is new Data_Pkg (Value_T => Integer);
   package Pkg_1 renames Pkg_H
   package Pkg_2 renames Pkg_1
   package Pkg_3 renames Pkg_2
   package Pkg_4 renames Pkg_3
   package Pkg_5 renames Pkg_4
   package Pkg renames Pkg_5;
end D_Data;
