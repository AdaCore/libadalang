with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;  use Libadalang.Helpers;

procedure Main is

   type Abort_Location is (Setup, In_Unit, Post_Process);
   Location : Abort_Location;

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array);
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);
   procedure App_Post_Process
     (Context : App_Context; Jobs : App_Job_Context_Array);

   package App is new Libadalang.Helpers.App
     (Name               => "test",
      Description        => "Test App",
      Enable_Parallelism => True,
      Process_Unit       => Process_Unit,
      App_Post_Process   => App_Post_Process);

   ---------------
   -- App_Setup --
   ---------------

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array) is
      pragma Unreferenced (Context, Jobs);
   begin
      if Location = Setup then
         Abort_App ("Abort in setup");
      end if;
   end App_Setup;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context, Unit);
   begin
      if Location = In_Unit then
         Abort_App;
      end if;
   end Process_Unit;

   ----------------------
   -- App_Post_Process --
   ----------------------

   procedure App_Post_Process
     (Context : App_Context; Jobs : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);
   begin
      --  Report job abortion status. If aborts happened during unit
      --  processing, which job exactly has aborted depends on task scheduling,
      --  and is thus non deterministic: just print a summary in this case.

      if Location = In_Unit then
         Put_Line ("At least one job has aborted: "
                   & Boolean'Image (for some J of Jobs => J.Aborted));
      else
         for J of Jobs loop
            Put_Line ("Has job" & J.ID'Image & " aborted: " & J.Aborted'Image);
         end loop;
      end if;

      if Location = Post_Process then
         Abort_App ("Abort in unit");
      end if;
   end App_Post_Process;

begin
   for Loc in Abort_Location loop
      Put_Line ("== " & Loc'Image & " ==");
      Location := Loc;
      App.Run;
      New_Line;
   end loop;
end Main;
