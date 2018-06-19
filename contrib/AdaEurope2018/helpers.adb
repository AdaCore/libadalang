with GNATCOLL.Projects;
with GNATCOLL.VFS;

with Libadalang.Unit_Files.Projects;

package body Helpers is
   package VFS renames GNATCOLL.VFS;

   package GPR renames GNATCOLL.Projects;

   function Initialize
     (Project_Path : String;
      Sources      : out Unit_Vectors.Vector) return LAL.Analysis_Context
   is

      Project : GPR.Project_Tree_Access;

      function Load_Project return LAL.Unit_Provider_Access;

      ------------------
      -- Load_Project --
      ------------------

      function Load_Project return LAL.Unit_Provider_Access is
         use Libadalang.Unit_Files.Projects;

         Env      : GPR.Project_Environment_Access;
         Provider : Project_Unit_Provider_Access;
      begin
         --  Load the project and create a unit provider wrapping it
         Project := new GPR.Project_Tree;
         GPR.Initialize (Env);
         Project.Load (VFS.Create (VFS."+" (Project_Path)));
         Provider := new Project_Unit_Provider_Type'
           (Create (Project, Env, True));

         return LAL.Unit_Provider_Access (Provider);
      end Load_Project;

      Provider : constant LAL.Unit_Provider_Access := Load_Project;
      Context  : constant LAL.Analysis_Context := LAL.Create
        (Unit_Provider => LAL.Unit_Provider_Access_Cst (Provider));
      Files    : VFS.File_Array_Access;

   begin
      --  Extract the list of source files to process from this project
      Files := Project.Root_Project.Source_Files;
      for F of Files.all loop
         declare
            Unit : constant LAL.Analysis_Unit :=
              LAL.Get_From_File (Context, VFS."+" (F.Full_Name));
         begin
            Sources.Append (Unit);
         end;
      end loop;
      VFS.Unchecked_Free (Files);

      return Context;
   end Initialize;

end Helpers;
