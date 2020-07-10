procedure Test_Target_Name is
   procedure Ignore_String (S : String) is null;
begin
   Ignore_String (Standard'Target_Name);
   pragma Test_Statement;
end Test_Target_Name;
