with Ada.Text_IO;         use Ada.Text_IO;
with Libadalang.Analysis; use Libadalang.Analysis;

procedure Main is
   Ctx       : Analysis_Context := Create;
   Unit      : Analysis_Unit := Get_From_File (Ctx, "test.adb");
begin
   Print (Unit);
   Destroy (Ctx);
   Put_Line ("Done.");
end Main;
