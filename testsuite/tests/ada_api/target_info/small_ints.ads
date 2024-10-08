package Small_Ints is

   pragma Compile_Time_Error
     (Integer'First /= -(2 **15), "Integer'First");
   pragma Compile_Time_Error
     (Integer'Last /= +(2 **15 - 1), "Integer'Last");

   pragma Compile_Time_Error
     (Natural'First /= 0, "Natural'First");
   pragma Compile_Time_Error
     (Natural'Last /= Integer'Last, "Natural'Last");

   pragma Compile_Time_Error
     (Positive'First /= 1, "Positive'First");
   pragma Compile_Time_Error
     (Positive'Last /= Integer'Last, "Positive'Last");

   pragma Compile_Time_Error
     (Short_Short_Integer'First /= -(2 **7), "Short_Short_Integer'First");
   pragma Compile_Time_Error
     (Short_Short_Integer'Last /= +(2 **7 - 1), "Short_Short_Integer'Last");

   pragma Compile_Time_Error
     (Short_Integer'First /= -(2 **7), "Short_Integer'First");
   pragma Compile_Time_Error
     (Short_Integer'Last /= +(2 **7 - 1), "Short_Integer'Last");

   pragma Compile_Time_Error
     (Long_Integer'First /= -(2 **31), "Long_Integer'First");
   pragma Compile_Time_Error
     (Long_Integer'Last /= +(2 **31 - 1), "Long_Integer'Last");

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

   pragma Compile_Time_Error
     (Short_Float'First /= -16#0.FFFF_FF#E32, "Short_Float'First");
   pragma Compile_Time_Error
     (Short_Float'Last /= 16#0.FFFF_FF#E32, "Short_Float'Last");

   pragma Compile_Time_Error (Float'First /= -16#0.FFFF_FF#E32, "Float'First");
   pragma Compile_Time_Error (Float'Last /= 16#0.FFFF_FF#E32, "Float'Last");

   pragma Compile_Time_Error
     (Long_Float'First /= -16#0.FFFF_FFFF_FFFF_F8#E256, "Long_Float'First");
   pragma Compile_Time_Error
     (Long_Float'Last /= 16#0.FFFF_FFFF_FFFF_F8#E256, "Long_Float'Last");

   pragma Compile_Time_Error
     (Long_Long_Float'First /= -16#0.FFFF_FFFF_FFFF_F8#E256,
       "Long_Long_Float'First");
   pragma Compile_Time_Error
     (Long_Long_Float'Last /= 16#0.FFFF_FFFF_FFFF_F8#E256,
       "Long_Long_Float'Last");

   pragma Compile_Time_Error
     (Duration'First /= -((2 ** 31) * 0.020), "Duration'First");
   pragma Compile_Time_Error
     (Duration'Last /= +((2 ** 31 - 1) * 0.020), "Duration'Last");

end Small_Ints;
