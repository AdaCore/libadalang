--  Test that the deallocation of a project unit provider does not attempt to
--  free the project when we don't expect it to.

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is
   Env  : Project_Environment_Access;
   Tree : Project_Tree_Access := new Project_Tree;
   UFP  : Unit_Provider_Reference;
begin
   Initialize (Env);
   Tree.Load (Create (+"p.gpr"), Env);
   UFP := Create_Project_Unit_Provider
     (Tree => Tree, Env => Env, Is_Project_Owner => False);

   Tree.Unload;
   Free (Tree);
   Free (Env);

   Put_Line ("Done.");
end Main;
