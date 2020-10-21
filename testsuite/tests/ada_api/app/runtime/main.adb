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
      Filename    : constant String := Unit.Get_Filename;
      Looking_For : constant String := "a-tags.ads";
   begin
      if Filename'Length > Looking_For'Length
         and then Filename (Filename'Last - Looking_For'Length + 1 
                              .. Filename'Last) = Looking_For
      then
         Put_Line ("Found " & Looking_For);
      end if;
   end Process_Unit;
begin
   App.Run;
end Main;
