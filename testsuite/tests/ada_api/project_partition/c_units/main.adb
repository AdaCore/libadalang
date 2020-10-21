--  Check that the project partitionner works on projects containing C units

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is
   Tree : Project_Tree_Access := new Project_Tree;
   Env  : Project_Environment_Access;
   PAPs : Provider_And_Projects_Array_Access;
begin
   Put_Line ("Loading the project:");
   Initialize (Env);
   Load (Tree.all, Create (+"ap.gpr"), Env);
   PAPs := Create_Project_Unit_Providers (Tree);
   for I in PAPs'Range loop
      Put ("  *");
      for P of PAPs (I).Projects.all loop
         Put (" ");
         Put (P.Name);
      end loop;
      New_Line;
   end loop;
   Free (PAPs);
   Free (Tree);
   Free (Env);
   Put_Line ("Done.");
end Main;
