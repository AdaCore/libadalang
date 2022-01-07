procedure P is
   function "-" (I: Integer) return Integer is (I*I);

   I : constant Integer := -3;
   J : constant := 14;
begin
   pragma Assert (I = 9);
   pragma Test_Statement;

   pragma Compile_Time_Warning (J < 15, "ha");
   pragma Test_Statement;

   pragma Compile_Time_Error (J < 15, "hi");
   pragma Test_Statement;

   pragma Assert (I = 9, (if I < 9 then "too low" else "too high"));
   pragma Test_Statement;
end P;
