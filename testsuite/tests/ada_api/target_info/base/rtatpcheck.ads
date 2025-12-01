package RTATPCheck is

   pragma Compile_Time_Error
     (Integer'First /= -(2 **15), "Integer'First");
   pragma Compile_Time_Error
     (Integer'Last /= +(2 **15 - 1), "Integer'Last");

end RTATPCheck;
