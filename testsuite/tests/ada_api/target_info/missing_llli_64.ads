package Missing_LLLI_64 is

   pragma Compile_Time_Error
     (Long_Long_Integer'First /= -(2 **63), "Long_Long_Integer'First");
   pragma Compile_Time_Error
     (Long_Long_Integer'Last /= +(2 **63 - 1), "Long_Long_Integer'Last");

   pragma Compile_Time_Error
     (Long_Long_Long_Integer'First /= -(2 **63),
       "Long_Long_Long_Integer'First");
   pragma Compile_Time_Error
     (Long_Long_Long_Integer'Last /= +(2 **63 - 1),
       "Long_Long_Long_Integer'Last");

end Missing_LLLI_64;
