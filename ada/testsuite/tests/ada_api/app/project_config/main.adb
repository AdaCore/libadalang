--  Check that App's --config argument works as expected

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

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

   Success   : Boolean;
   GPRconfig : String_Access := Locate_Exec_On_Path ("gprconfig");
   Args      : String_List_Access := new String_List'
     (new String'("-o"),
      new String'("foo.cgpr"),
      new String'("--batch"));
begin
   Spawn (GPRconfig.all, Args.all, Success);
   Free (GPRconfig);
   Free (Args);
   if not Success then
      raise Program_Error with "Call to gprconfig failed";
   end if;
   App.Run;
end Main;
