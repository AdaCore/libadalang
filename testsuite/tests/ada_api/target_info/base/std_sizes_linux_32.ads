package Std_Sizes_Linux_32 is
   pragma Compile_Time_Error
     (Boolean'Size /= 1, "Boolean'Size");
   pragma Compile_Time_Error
     (Character'Size /= 8, "Character'Size");
   pragma Compile_Time_Error
     (Float'Size /= 32, "Float'Size");
   pragma Compile_Time_Error
     (Long_Float'Size /= 64, "Long_Float'Size");
   pragma Compile_Time_Error
     (Long_Long_Float'Size /= 96, "Long_Long_Float'Size");
   pragma Compile_Time_Error
     (Short_Integer'Size /= 16, "Short_Integer'Size");
   pragma Compile_Time_Error
     (Integer'Size /= 32, "Integer'Size");
   pragma Compile_Time_Error
     (Long_Integer'Size /= 32, "Long_Integer'Size");
   pragma Compile_Time_Error
     (Long_Long_Integer'Size /= 64, "Long_Long_Integer'Size");
   pragma Compile_Time_Error
     (Long_Long_Long_Integer'Size /= 64, "Long_Long_Long_Integer'Size");
end Std_Sizes_Linux_32;
