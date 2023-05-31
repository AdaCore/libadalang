with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

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
      --  Without --sort-by-basename, the source files to process would be
      --  sorted by full name, and thus src1/p2.ads would be processed before
      --  src3/p1.ads. With --sort-by-basename, src3/p1.ads should be first.

      Put_Line (Simple_Name (Unit.Get_Filename));
   end Process_Unit;
begin
   App.Run;
end Main;
