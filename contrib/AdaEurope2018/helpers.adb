with GNATCOLL.Projects;
with GNATCOLL.VFS;

with Libadalang.Project_Provider;

package body Helpers is
   package VFS renames GNATCOLL.VFS;

   package GPR renames GNATCOLL.Projects;

   function Initialize
     (Project_Path : String;
      Sources      : out Unit_Vectors.Vector) return LAL.Analysis_Context
   is

      Project : GPR.Project_Tree_Access;

      function Load_Project return LAL.Unit_Provider_Reference;

      ------------------
      -- Load_Project --
      ------------------

      function Load_Project return LAL.Unit_Provider_Reference is
         use Libadalang.Project_Provider;

         Env : GPR.Project_Environment_Access;
      begin
         --  Load the project and create a unit provider wrapping it
         Project := new GPR.Project_Tree;
         GPR.Initialize (Env);
         Project.Load (VFS.Create (VFS."+" (Project_Path)));

         return Create_Project_Unit_Provider
           (Project, Env => Env, Is_Project_Owner => True);
      end Load_Project;

      Provider : constant LAL.Unit_Provider_Reference := Load_Project;
      Context  : constant LAL.Analysis_Context := LAL.Create_Context
        (Unit_Provider => Provider);
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
