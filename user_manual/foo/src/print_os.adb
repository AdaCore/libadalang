with Ada.Text_IO; use Ada.Text_IO;

with OS;

procedure Print_OS is
begin
   Put_Line (OS.Name);
end Print_OS;
