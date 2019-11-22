with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;  use Libadalang.Helpers;

procedure Main is

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);

   package App is new Libadalang.Helpers.App
     ("Test App for the auto provider",
      Process_Unit => Process_Unit);

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);
      File : constant String := +Create (+Unit.Get_Filename).Base_Name;
   begin
      Put_Line ("Processing: " & File);
   end Process_Unit;

begin
   App.Run;
end Main;
