--  Check that App reports GPR missing directories by default with --gpr2

with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;  use Libadalang.Helpers;

procedure Main is
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);

   package App is new Libadalang.Helpers.App
     (Name         => "example",
      Description  => "Example app",
      Process_Unit => Process_Unit);

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);
   begin
      Put_Line (Unit.Root.Image);
   end Process_Unit;

begin
   App.Run;
end Main;
