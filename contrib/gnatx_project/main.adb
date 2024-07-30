with Ada.Text_IO;                     use Ada.Text_IO;

with Libadalang.Analysis;             use Libadalang.Analysis;
with Libadalang.Helpers;              use Libadalang.Helpers;

procedure Main is
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);

   package App is new Libadalang.Helpers.App
     (Name                   => "test_gnatx_app",
      Description            =>
         "Test that we can create an app using -gnatX and LAL",
      Process_Unit           => Process_Unit);

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
