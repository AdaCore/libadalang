with Data_Pkg;

package D_Data is
   package Pkg_H is new Data_Pkg (Value_T => Integer);
   package Pkg renames Pkg_H;
end D_Data;
