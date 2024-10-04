--  Check that the project partitionner works on projects containing C units

with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is
   Options : GPR2.Options.Object;
   Tree    : GPR2.Project.Tree.Object;
   PAPs    : GPR2_Provider_And_Projects_Array_Access;
begin
   Put_Line ("Loading the project:");
   Options.Add_Switch (GPR2.Options.P, "ap.gpr");
   if not Tree.Load
     (Options,
      With_Runtime         => True,
      Artifacts_Info_Level => GPR2.Sources_Units)
   then
      raise Program_Error;
   end if;
   PAPs := Create_Project_Unit_Providers (Tree);
   for I in PAPs'Range loop
      Put ("  *");
      for View of PAPs (I).Projects loop
         Put (" ");
         Put (String (View.Name));
      end loop;
      New_Line;
   end loop;
   Free (PAPs);
   Put_Line ("Done.");
end Main;
