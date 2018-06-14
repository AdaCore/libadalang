with Ada.Containers.Vectors;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with Libadalang.Unit_Files.Projects;

package body Helpers is

   package GPR renames GNATCOLL.Projects;
   package VFS renames GNATCOLL.VFS;

   package Filename_Vectors is new Ada.Containers.Vectors
     (Positive, VFS.Virtual_File, "=" => VFS."=");

   procedure Iterate_Units
     (Initialize : access procedure (Context : LAL.Analysis_Context) := null;
      Process    : not null access procedure (Unit : LAL.Analysis_Unit);
      Summarize  : access procedure (Context : LAL.Analysis_Context) := null)
   is

      Project : GPR.Project_Tree_Access;
      Sources : Filename_Vectors.Vector;

      function Load_Project return LAL.Unit_Provider_Access;

      ------------------
      -- Load_Project --
      ------------------

      function Load_Project return LAL.Unit_Provider_Access is
         use Libadalang.Unit_Files.Projects;

         Env      : GPR.Project_Environment_Access;
         Provider : Project_Unit_Provider_Access;
         Files    : VFS.File_Array_Access;
      begin
         --  Load the "material.gpr" project and create a unit provider
         --  wrapping it.
         Project := new GPR.Project_Tree;
         GPR.Initialize (Env);
         Project.Load (VFS.Create (VFS."+" ("material.gpr")));
         Provider := new Project_Unit_Provider_Type'
           (Create (Project, Env, True));

         --  Extract the list of source files to process from this project
         Files := Project.Root_Project.Source_Files;
         for F of Files.all loop
            Sources.Append (F);
         end loop;
         VFS.Unchecked_Free (Files);

         return LAL.Unit_Provider_Access (Provider);
      end Load_Project;

      Provider : LAL.Unit_Provider_Access := Load_Project;
      Context  : LAL.Analysis_Context := LAL.Create
        (Unit_Provider => LAL.Unit_Provider_Access_Cst (Provider));
   begin

      if Initialize /= null then
         Initialize (Context);
      end if;

      for F of Sources loop
         declare
            Unit : constant LAL.Analysis_Unit :=
              LAL.Get_From_File (Context, VFS."+" (F.Full_Name));
         begin
            Process (Unit);
         end;
      end loop;

      if Summarize /= null then
         Summarize (Context);
      end if;

      LAL.Destroy (Context);
      LAL.Destroy (Provider);
   end Iterate_Units;

end Helpers;
