with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Target_Name is
begin
   Put_Line (Standard'Target_Name);
   pragma Test_Statement;
end Test_Target_Name;
