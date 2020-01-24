--  Check how the project provider constructor builds partition of aggregated
--  projects.

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Traces;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Iterators;        use Libadalang.Iterators;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is

   package LAL renames Libadalang.Analysis;

   type Context_Array is array (Positive range <>) of Analysis_Context;

   Tree : Project_Tree_Access := new Project_Tree;
   Env  : Project_Environment_Access;
   PAPs : Provider_And_Projects_Array_Access;

begin
   GNATCOLL.Traces.Parse_Config ("LIBADALANG.PROJECT_PROVIDER=yes");
   Put_Line ("Loading the project:");
   Initialize (Env);
   Load (Tree.all, Create (+"ap1.gpr"), Env);
   PAPs := Create_Project_Unit_Providers (Tree);

   declare
      Contexts : Context_Array (PAPs'Range);
   begin
      for I in PAPs'Range loop
         Contexts (I) := Create_Context (Unit_Provider => PAPs (I).Provider);
         Put ("  *");
         for P of PAPs (I).Projects.all loop
            Put (" ");
            Put (P.Name);
         end loop;
         New_Line;
      end loop;
      New_Line;
      Free (PAPs);

      declare
         Unit : constant Analysis_Unit :=
            Contexts (1).Get_From_Provider ("p2", Unit_Specification);
         Ref  : constant LAL.Name :=
            Find_First (Unit.Root, Kind_Is (Ada_Object_Decl))
            .As_Object_Decl.F_Renaming_Clause.F_Renamed_Object;
         Decl : constant Basic_Decl := Ref.P_Referenced_Decl;
      begin
         Put_Line (Ref.Short_Image & " resolves to " & Decl.Short_Image);
      end;
   end;

   Free (Tree);
   Free (Env);
   Put_Line ("Done.");
end Main;
