package Std_Attrs_Missing_LLLI is
   pragma Compile_Time_Error
     (Standard'Max_Integer_Size /= 32, "Standard'Max_Integer_Size");
end Std_Attrs_Missing_LLLI;

