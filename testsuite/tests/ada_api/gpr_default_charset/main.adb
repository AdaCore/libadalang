--  Check that Libadalang.Project_Provider.Default_Charset_From_Project
--  funtions work as expected.

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GPR2.Options;
with GPR2.Project.Tree;

with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is

   procedure Check (Project : String);
   --  Run default charset detection on the given project file

   -----------
   -- Check --
   -----------

   procedure Check (Project : String) is
   begin
      Put_Line ("== " & Project & " ==");
      New_Line;

      declare
         Env  : Project_Environment_Access;
         Tree : Project_Tree;
      begin
         Initialize (Env);
         Tree.Load (Create (+Project), Env);
         Put_Line ("GPR1: " & Default_Charset_From_Project (Tree));
         Tree.Unload;
         Free (Env);
      end;

      declare
         Options : GPR2.Options.Object;
         Tree    : GPR2.Project.Tree.Object;
      begin
         Options.Add_Switch (GPR2.Options.P, Project);
         if not Tree.Load
           (Options,
            With_Runtime         => True,
            Artifacts_Info_Level => GPR2.Sources_Units)
         then
            raise Program_Error;
         end if;
         Put_Line ("GPR2: " & Default_Charset_From_Project (Tree));
      end;

      New_Line;
   end Check;

begin
   Check ("default.gpr");
   Check ("utf8.gpr");
   Put_Line ("Done.");
end Main;
