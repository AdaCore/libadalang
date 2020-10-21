with Ada.Text_IO;         use Ada.Text_IO;
with Libadalang.Analysis; use Libadalang.Analysis;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context;
   Unit : constant Analysis_Unit := Get_From_File (Ctx, "test.adb");
begin
   Print (Unit);
   Put_Line ("Done.");
end Main;
