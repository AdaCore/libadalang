package Missing_LLLI_32 is

   pragma Compile_Time_Error
     (Long_Long_Integer'First /= -(2 **31), "Long_Long_Integer'First");
   pragma Compile_Time_Error
     (Long_Long_Integer'Last /= +(2 **31 - 1), "Long_Long_Integer'Last");

   pragma Compile_Time_Error
     (Long_Long_Long_Integer'First /= -(2 **31),
       "Long_Long_Long_Integer'First");
   pragma Compile_Time_Error
     (Long_Long_Long_Integer'Last /= +(2 **31 - 1),
       "Long_Long_Long_Integer'Last");

end Missing_LLLI_32;
