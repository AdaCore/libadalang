package Std_Sizes_Missing_LLLI_32 is
   pragma Compile_Time_Error
     (Long_Long_Integer'Size /= 32, "Long_Long_Integer'Size");
   pragma Compile_Time_Error
     (Long_Long_Long_Integer'Size /= 32, "Long_Long_Long_Integer'Size");
end Std_Sizes_Missing_LLLI_32;
