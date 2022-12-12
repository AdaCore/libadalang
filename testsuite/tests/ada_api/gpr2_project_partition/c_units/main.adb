--  Check that the project partitionner works on projects containing C units

with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;

with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   PAPs : GPR2_Provider_And_Projects_Array_Access;
begin
   Put_Line ("Loading the project:");
   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File ("ap.gpr"),
      Context  => GPR2.Context.Empty);
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
