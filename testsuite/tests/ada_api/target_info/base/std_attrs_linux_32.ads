package Std_Attrs_Linux_32 is
   pragma Compile_Time_Error
     (Standard'Max_Integer_Size /= 64, "Standard'Max_Integer_Size");

   pragma Compile_Time_Error
     (Standard'Address_Size /= 32, "Standard'Address_Size");

   pragma Compile_Time_Error
     (Standard'Word_Size /= 32, "Standard'Word_Size");

   pragma Compile_Time_Error
     (Standard'Storage_Unit /= 8, "Standard'Storage_Unit");

   pragma Compile_Time_Error
     (Standard'Maximum_Alignment /= 16, "Standard'Maximum_Alignment");

   pragma Compile_Time_Error
     (Standard'System_Allocator_Alignment /= 8,
      "Standard'System_Allocator_Alignment");

   pragma Compile_Time_Error
     (Standard'Wchar_T_Size /= 32, "Standard'Wchar_T_Size");
end Std_Attrs_Linux_32;
