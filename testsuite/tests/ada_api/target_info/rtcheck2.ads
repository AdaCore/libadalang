package RTCheck2 is

   pragma Compile_Time_Error
     (Integer'First /= -(2 **31), "Integer'First");
   pragma Compile_Time_Error
     (Integer'Last /= +(2 **31 - 1), "Integer'Last");

end RTCheck2;
