package Std_Attrs_Linux_64 is
   pragma Compile_Time_Error
     (Standard'Max_Integer_Size /= 128, "Standard'Max_Integer_Size");

   pragma Compile_Time_Error
     (Standard'Address_Size /= 64, "Standard'Address_Size");

   pragma Compile_Time_Error
     (Standard'Word_Size /= 64, "Standard'Word_Size");

   pragma Compile_Time_Error
     (Standard'Storage_Unit /= 8, "Standard'Storage_Unit");

   pragma Compile_Time_Error
     (Standard'Maximum_Alignment /= 16, "Standard'Maximum_Alignment");

   pragma Compile_Time_Error
     (Standard'System_Allocator_Alignment /= 16,
      "Standard'System_Allocator_Alignment");

   pragma Compile_Time_Error
     (Standard'Wchar_T_Size /= 32, "Standard'Wchar_T_Size");
end Std_Attrs_Linux_64;
