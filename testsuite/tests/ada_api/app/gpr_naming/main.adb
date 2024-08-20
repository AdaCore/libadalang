--  Check that naming clauses in GPR files actually means that some source
--  files (pkg__64.ads) are ignored.

with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;  use Libadalang.Helpers;

procedure Main is

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);

   package App is new Libadalang.Helpers.App
     (Name         => "example",
      Description  => "Example app. Print units that are processed by App.",
      Process_Unit => Process_Unit);

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);
   begin
      Put_Line
        ("Processing " & Ada.Directories.Simple_Name (Unit.Get_Filename));
   end Process_Unit;
begin
   App.Run;
end Main;
