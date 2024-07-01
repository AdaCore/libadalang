--  Check that ``Libadalang.Config_Pragmas.Import_From_Project`` works as
--  expected on various project setups.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Config_Pragmas;   use Libadalang.Config_Pragmas;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Projects is

   Ctx : constant Analysis_Context := Create_Context;

   procedure Check_Unit (Filename : Virtual_File);
   --  Print the name of the given source file and the list of pragmas that
   --  apply to the corresponding unit.

   procedure Run (Root_Project, Project : String);
   --  Load ``Root_Project``, then import its configuration pragmas files into
   --  ``Ctx`` (starting at the ``Project`` subproject if a non-empty string is
   --  passed), then check the list of pragmas that apply to all sources in the
   --  project.

   ----------------
   -- Check_Unit --
   ----------------

   procedure Check_Unit (Filename : Virtual_File) is
      CU      : constant Compilation_Unit :=
        Ctx.Get_From_File (+Filename.Full_Name).Root.As_Compilation_Unit;
      Pragmas : constant Pragma_Node_Array := CU.P_All_Config_Pragmas;
   begin
      Put ("   " & (+Filename.Base_Name) & ":");
      for P of Pragmas loop
         Put (" " & Image (P.F_Id.Text));
      end loop;
      New_Line;
   end Check_Unit;

   ---------
   -- Run --
   ---------

   procedure Run (Root_Project, Project : String) is
      Env : Project_Environment_Access;
      Prj : Project_Tree;
      P   : Project_Type;
      It  : Project_Iterator;

      Mapping : Config_Pragmas_Mapping;
   begin
      Put_Line ("## " & Root_Project);
      if Project /= "" then
         Put_Line ("   (starting at " & Project & ")");
      end if;
      New_Line;

      --  Load the given project and import configuration pragmas files from it
      --  or one of its sub-projects.

      Initialize (Env);
      Prj.Load (Create (+Root_Project), Env => Env, Errors => Put_Line'Access);
      P := (if Project = ""
            then No_Project
            else Prj.Project_From_Name (Project));
      Mapping := Import_From_Project (Ctx, Prj, P);
      Set_Mapping (Ctx, Mapping);

      --  Make sure we get the same mapping when loading from a GPR2 project

      declare
         Options      : GPR2.Options.Object;
         Tree         : GPR2.Project.Tree.Object;
         View         : GPR2.Project.View.Object := GPR2.Project.View.Undefined;
         GPR2_Mapping : Config_Pragmas_Mapping;
      begin
         Options.Add_Switch (GPR2.Options.P, Root_Project);
         if not Tree.Load (Options, With_Runtime => True)
            or else not Update_Sources (Tree)
         then
            raise Program_Error;
         end if;
         if Project /= "" then
            for V of Tree.Ordered_Views loop
               if To_Lower (String (V.Name)) = To_Lower (Project) then
                  View := V;
               end if;
            end loop;
            pragma Assert (View.Is_Defined);
         end if;

         GPR2_Mapping := Import_From_Project (Ctx, Tree, View);
         if Mapping /= GPR2_Mapping then
            Dump (GPR2_Mapping);
            raise Program_Error with "inconsistent results from GPR2";
         end if;
      end;

      --  Now check the configuration pragmas for all sources in the project
      --  tree.

      It := Prj.Root_Project.Start;
      while Current (It) /= No_Project loop
         Put_Line (".. " & Current (It).Name);
         declare
            Files : File_Array_Access :=
              Current (It).Extended_Projects_Source_Files;
         begin
            Sort (Files.all);
            for F of Files.all loop
               Check_Unit (F);
            end loop;
            Unchecked_Free (Files);
         end;
         Next (It);
      end loop;
      New_Line;

      Prj.Unload;
      Free (Env);
   end Run;

begin
   --  This checks that both local and global configuration pragmas are
   --  correctly picked up:
   --
   --  * local ones (Local_A, Local_B) apply to the sources of the project that
   --    defines it;
   --  * global ones (Global_C) apply to the closure of the project that
   --    defines it, but only for the root project.

   Run ("gpr/c.gpr", "");

   --  Check that the global configuration pragmas files are ignored when
   --  defined in subprojects but not in the root project.

   Run ("gpr/b.gpr", "");

   --  For extending projects:
   --
   --  * If the extending project has a Compiler package that defines a
   --    Local_Configuration_Pragmas attribute, use it for all units in this
   --    project (ext_a1.gpr).
   --
   --  * If the extending project has a Compiler package that defines no such
   --    attribute, no unit in that project have local configuration pragmas
   --    (ext_a2.gpr).
   --
   --  * If the extending project does not define a Compiler package, all units
   --    in that project use the local configuration pragmas from the extended
   --    project attribute (ext_a3.gpr).

   Run ("gpr/ext_a1.gpr", "");
   Run ("gpr/ext_a2.gpr", "");
   Run ("gpr/ext_a3.gpr", "");

   --  Check the handling of aggregate projects

   Run ("gpr/agg.gpr", "");

   --  Check that we can start from a subproject, and still use global
   --  configuration pragmas files from the root project.

   Run ("gpr/c.gpr", "a");

   Put_Line ("Done.");
end Projects;
