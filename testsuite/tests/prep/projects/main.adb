with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Langkit_Support.Errors;   use Langkit_Support.Errors;
with Libadalang.Preprocessing; use Libadalang.Preprocessing;

procedure Main is

   type Project_Names is array (Positive range <>) of Unbounded_String;

   procedure Process
     (Filename : String;
      Projects : Project_Names := (1 => Null_Unbounded_String));
   --  Extract preprocessor data from the Filename project, starting extraction
   --  from the subprojects listed in Projects.

   -------------
   -- Process --
   -------------

   procedure Process
     (Filename : String;
      Projects : Project_Names := (1 => Null_Unbounded_String))
   is
      Env     : Project_Environment_Access;
      Tree    : Project_Tree;
      Project : Project_Type;

      GPR2_Tree : GPR2.Project.Tree.Object;
      GPR2_View : GPR2.Project.View.Object;
   begin
      Put_Line ("== " & Filename & " ==");
      New_Line;

      --  Load the project, both using GNATCOLL.Projects and GPR2

      Initialize (Env);
      Tree.Load (Create (+Filename));

      GPR2_Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
                       (GPR2.Filename_Type (Filename),
                        GPR2.Path_Name.No_Resolution),
         Context  => GPR2.Context.Empty);

      --  Run the extraction on all the requested subprojects

      for Project_Name of Projects loop

         --  Fetch the requested subproject

         Project := No_Project;
         GPR2_View := GPR2.Project.View.Undefined;
         declare
            Name : constant String := To_String (Project_Name);
         begin
            if Project_Name /= "" then
               Put_Line
                 ("Focusing on the " & Name & " sub-project");

               Project := Tree.Project_From_Name (Name);
               pragma Assert (Project /= No_Project);

               for View of GPR2_Tree.Ordered_Views loop
                  if To_Lower (String (View.Name)) = To_Lower (Name) then
                     GPR2_View := View;
                     exit;
                  end if;
               end loop;
               pragma Assert (GPR2_View.Is_Defined);

               New_Line;
            end if;
         end;

         --  Do the extraction, both using the GPR1 and the GPR2 APIs. Make
         --  sure we get the same data in both cases.

         begin
            declare
               GPR1_Data : constant Preprocessor_Data :=
                 Extract_Preprocessor_Data_From_Project (Tree, Project);
               GPR2_Data : constant Preprocessor_Data :=
                 Extract_Preprocessor_Data_From_Project (GPR2_Tree, GPR2_View);
            begin
               Dump (GPR1_Data);

               if GPR1_Data /= GPR2_Data then
                  Put_Line ("Inconsistent results for GPR2:");
                  Dump (GPR2_Data);
                  raise Program_Error;
               end if;
            end;
         exception
            when Exc : File_Read_Error | Syntax_Error =>
               Put_Line
                 (Exception_Name (Exc) & ": " & Exception_Message (Exc));
               New_Line;
         enD;
      end loop;

      Tree.Unload;
      Free (Env);
   end Process;

begin
   Process ("simple/p.gpr");
   Process ("switches/p.gpr");
   Process ("with/q.gpr");
   Process ("extends/q.gpr");
   Process
     ("aggr/aggr.gpr",
      (Null_Unbounded_String,
       To_Unbounded_String ("p"),
       To_Unbounded_String ("q")));
   Process ("file_read_error/p.gpr");
   Process ("syntax_error/p.gpr");
   Process ("gnatep_in_source_dirs/p.gpr");
   Put_Line ("Done.");
end Main;
