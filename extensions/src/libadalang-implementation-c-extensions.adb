--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces.C.Strings; use Interfaces.C.Strings;

with GNAT.Task_Lock;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;
with GNATCOLL.VFS;        use GNATCOLL.VFS;

with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

with Libadalang.Analysis;          use Libadalang.Analysis;
with Libadalang.Auto_Provider;     use Libadalang.Auto_Provider;
with Libadalang.Preprocessing;     use Libadalang.Preprocessing;
with Libadalang.Project_Provider;  use Libadalang.Project_Provider;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Implementation.C.Extensions is

   function To_C_Provider
     (Provider : Unit_Provider_Reference) return ada_unit_provider
   is (Wrap_Private_Provider (Wrap_Public_Provider (Provider)));

   function Scenario_Vars_Count
     (Scenario_Vars : System.Address) return Natural;
   --  Return the number of scenario variables in the Scenario_Vars C-style
   --  array. This counts the number of entries before the first NULL entry.

   procedure Load_Project
     (Project_File    : chars_ptr;
      Scenario_Vars   : System.Address;
      Target, Runtime : chars_ptr;
      Tree            : out Project_Tree_Access;
      Env             : out Project_Environment_Access);
   --  Helper to load a project file from C arguments. May propagate a
   --  ``GNATCOLL.Projects.Invalid_Project`` exception if the project cannot be
   --  loaded.

   function Create_Unit_Provider
     (Tree             : Project_Tree_Access;
      Env              : Project_Environment_Access;
      Project          : chars_ptr;
      Is_Project_Owner : Boolean) return ada_unit_provider;
   --  Helper to create a unit provider from a loaded project file.  May
   --  propagate a ``GNATCOLL.Projects.Invalid_Project`` exception if a
   --  specific sub-project is requested but does not exist, or an
   --  ``Unsupported_View_Error`` exception if the requested project contains
   --  conflicting source files.

   -------------------------
   -- Scenario_Vars_Count --
   -------------------------

   function Scenario_Vars_Count (Scenario_Vars : System.Address) return Natural
   is
      Result : Natural := 1;
      SV     : ada_gpr_project_scenario_variable_array (Positive)
         with Import  => True,
              Address => Scenario_Vars;
   begin
      loop
         exit when SV (Result).Name = Null_Ptr;
         Result := Result + 1;
      end loop;
      return Result - 1;
   end Scenario_Vars_Count;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Project_File    : chars_ptr;
      Scenario_Vars   : System.Address;
      Target, Runtime : chars_ptr;
      Tree            : out Project_Tree_Access;
      Env             : out Project_Environment_Access)
   is
      PF            : constant String := Value (Project_File);
      Target_Value  : constant String :=
        (if Target = Null_Ptr then "" else Value (Target));
      Runtime_Value : constant String :=
        (if Runtime = Null_Ptr then "" else Value (Runtime));
   begin
      GNAT.Task_Lock.Lock;

      --  Initialize the environment (target, runtime, externals) before
      --  loading the project.

      Tree := new Project_Tree;
      Initialize (Env);
      Env.Set_Target_And_Runtime (Target_Value, Runtime_Value);
      if Scenario_Vars /= System.Null_Address then
         declare
            Vars : ada_gpr_project_scenario_variable_array
                     (1 .. Scenario_Vars_Count (Scenario_Vars))
               with Import  => True,
                    Address => Scenario_Vars;
         begin
            for V of Vars loop
               Change_Environment (Env.all, Value (V.Name), Value (V.Value));
            end loop;
         end;
      end if;

      --  Try to load the project

      begin
         Load (Self                => Tree.all,
               Root_Project_Path   => Create (+PF),
               Env                 => Env,
               Report_Missing_Dirs => False);
      exception
         when Invalid_Project =>
            Free (Tree);
            Free (Env);
            raise;
      end;

      GNAT.Task_Lock.Unlock;

   exception
      when others =>
         GNAT.Task_Lock.Unlock;
         raise;
   end Load_Project;

   --------------------------
   -- Create_Unit_Provider --
   --------------------------

   function Create_Unit_Provider
     (Tree             : Project_Tree_Access;
      Env              : Project_Environment_Access;
      Project          : chars_ptr;
      Is_Project_Owner : Boolean) return ada_unit_provider
   is
      Prj : Project_Type := No_Project;
      P   : constant String :=
        (if Project = Null_Ptr then "" else Value (Project));
   begin
      if P /= "" then
         --  A specific project was requested: try to build one unit
         --  provider just for it. Lookup the project by name, and then if
         --  not found, by path.
         Prj := Tree.Project_From_Name (P);
         if Prj = No_Project then
            Prj := Tree.Project_From_Path (Create (+P));
         end if;
         if Prj = No_Project then
            raise GNATCOLL.Projects.Invalid_Project
               with "no such project: " & P;
         end if;
      end if;

      return To_C_Provider
        (Create_Project_Unit_Provider (Tree, Prj, Env, Is_Project_Owner));
   end Create_Unit_Provider;

   --------------------------
   -- ada_gpr_project_load --
   --------------------------

   function ada_gpr_project_load
     (Project_File    : chars_ptr;
      Scenario_Vars   : System.Address;
      Target, Runtime : chars_ptr) return ada_gpr_project_ptr
   is
      Project : Project_Tree_Access;
      Env     : Project_Environment_Access;
   begin
      Clear_Last_Exception;

      Load_Project
        (Project_File, Scenario_Vars, Target, Runtime, Project, Env);
      return new ada_gpr_project'(Project, Env);

   exception
      when Exc : GNATCOLL.Projects.Invalid_Project =>
         Set_Last_Exception (Exc);
         return null;
   end ada_gpr_project_load;

   --------------------------
   -- ada_gpr_project_free --
   --------------------------

   procedure ada_gpr_project_free (Self : ada_gpr_project_ptr) is
      Var_Self : ada_gpr_project_ptr := Self;
   begin
      Clear_Last_Exception;

      Self.Tree.Unload;
      Free (Self.Tree);
      Free (Self.Env);
      Free (Var_Self);
   end ada_gpr_project_free;

   ------------------------------------------
   -- ada_gpr_project_create_unit_provider --
   ------------------------------------------

   function ada_gpr_project_create_unit_provider
     (Self    : ada_gpr_project_ptr;
      Project : chars_ptr) return ada_unit_provider
   is
   begin
      Clear_Last_Exception;
      return Create_Unit_Provider (Self.Tree, Self.Env, Project, False);
   exception
      when Exc : Unsupported_View_Error | GNATCOLL.Projects.Invalid_Project =>
         Set_Last_Exception (Exc);
         return ada_unit_provider (System.Null_Address);
   end ada_gpr_project_create_unit_provider;

   ------------------------------
   -- ada_create_auto_provider --
   ------------------------------

   function ada_create_auto_provider
     (Input_Files : System.Address;
      Charset     : chars_ptr)
      return ada_unit_provider
   is
      type C_String_Array is array (Positive) of chars_ptr
         with Convention => C;

      Files_Count       : Natural := 0;
      Input_Files_Array : C_String_Array with
         Import  => True,
         Address => Input_Files;

      Actual_Charset : constant String :=
        (if Charset = Null_Ptr then Default_Charset else Value (Charset));
   begin
      while Input_Files_Array (Files_Count + 1) /= Null_Ptr loop
         Files_Count := Files_Count + 1;
      end loop;

      declare
         --  Allocate the array of filenames on the heap, as it may be too
         --  large for the stack.

         Files : File_Array_Access := new File_Array (1 .. Files_Count);
      begin
         for I in Files'Range loop
            Files (I) := Create (+Value (Input_Files_Array (I)));
         end loop;

         return Provider : constant ada_unit_provider := To_C_Provider
           (Create_Auto_Provider_Reference (Files.all, Actual_Charset))
         do
            Unchecked_Free (Files);
         end return;
      end;
   end ada_create_auto_provider;

   ----------------------------------
   -- ada_gpr_project_source_files --
   ----------------------------------

   function ada_gpr_project_source_files
     (Self : ada_gpr_project_ptr;
      Mode : int) return Source_File_Array_Ref_Access
   is
      M      : Source_Files_Mode;
      Result : Filename_Vectors.Vector;
   begin
      Clear_Last_Exception;

      --  Decode the ``Mode`` argument

      begin
         M := Source_Files_Mode'Val (Mode);
      exception
         when Exc : Constraint_Error =>
            Set_Last_Exception (Exc);
            return null;
      end;

      --  Compute the list of source files

      Result := Source_Files (Self.Tree.all, M);

      --  Convert the vector to the C API result

      declare
         Ref : constant Source_File_Array_Ref_Access :=
           new Source_File_Array_Ref (int (Result.Length));
         I   : int := 1;
      begin
         Ref.C_Ptr := Ref.Items'Address;
         for F of Result loop
            Ref.Items (I) := New_String (Ada.Strings.Unbounded.To_String (F));
            I := I + 1;
         end loop;

         return Ref;
      end;
   end ada_gpr_project_source_files;

   ---------------------------------------
   -- ada_gpr_project_free_source_files --
   ---------------------------------------

   procedure ada_gpr_project_free_source_files
     (Source_Files : Source_File_Array_Ref_Access)
   is
      S : Source_File_Array_Ref_Access;
   begin
      for F of Source_Files.Items loop
         Free (F);
      end loop;
      S := Source_Files;
      Free (S);
   end ada_gpr_project_free_source_files;

   --------------------------------------
   -- ada_create_project_unit_provider --
   --------------------------------------

   function ada_create_project_unit_provider
     (Project_File, Project : chars_ptr;
      Scenario_Vars         : System.Address;
      Target, Runtime       : chars_ptr) return ada_unit_provider
   is
      Tree : Project_Tree_Access;
      Env  : Project_Environment_Access;
   begin
      Clear_Last_Exception;

      Load_Project (Project_File, Scenario_Vars, Target, Runtime, Tree, Env);
      return Create_Unit_Provider (Tree, Env, Project, True);

   exception
      when Exc : Unsupported_View_Error | GNATCOLL.Projects.Invalid_Project =>
         Set_Last_Exception (Exc);
         if Tree /= null then
            Tree.Unload;
            Free (Tree);
            Free (Env);
         end if;
         return ada_unit_provider (System.Null_Address);
   end ada_create_project_unit_provider;

   ---------------------------------------
   -- ada_create_preprocessor_from_file --
   ---------------------------------------

   function ada_create_preprocessor_from_file
     (Filename    : chars_ptr;
      Path_Data   : access chars_ptr;
      Path_Length : int;
      Line_Mode   : access int) return ada_file_reader
   is
      Path   : Any_Path;
      FR_Ref : File_Reader_Reference;
   begin
      Clear_Last_Exception;

      --  Convert the given path data into an ``Any_Path`` value. To do this,
      --  concatenate all of its strings separated with NUL bytes and let
      --  ``Parse_Path`` split it back.

      declare
         Path_Array : array (1 .. Path_Length) of chars_ptr
         with Import, Address => Path_Data.all'Address;

         Path_Str : Unbounded_String;
      begin
         for I in Path_Array'Range loop
            if I > 1 then
               Append (Path_Str, ASCII.NUL);
            end if;
            Append (Path_Str, Value (Path_Array (I)));
         end loop;

         Path := Parse_Path
           (To_String (Path_Str), Separator => ASCII.NUL, CWD => If_Empty);
      end;

      --  Now create the file reader and wrap it to get the correct return type

      begin
         if Line_Mode = null then
            FR_Ref := Create_Preprocessor_From_File (Value (Filename), Path);
         else
            FR_Ref := Create_Preprocessor_From_File
              (Value (Filename), Path, Any_Line_Mode'Val (Line_Mode.all));
         end if;
      exception
         when Exc : File_Read_Error | Syntax_Error =>
            Set_Last_Exception (Exc);
            return ada_file_reader (System.Null_Address);
      end;

      declare
         FR_Int : constant Internal_File_Reader_Access :=
           Wrap_Public_File_Reader (FR_Ref);
      begin
         return Wrap_Private_File_Reader (FR_Int);
      end;
   end ada_create_preprocessor_from_file;

   -----------------------------------------
   -- ada_gpr_project_create_preprocessor --
   -----------------------------------------

   function ada_gpr_project_create_preprocessor
     (Self : ada_gpr_project_ptr) return ada_file_reader
   is
      Default_Config : File_Config;
      File_Configs   : File_Config_Maps.Map;
   begin
      Clear_Last_Exception;

      --  Load preprocessor data from the given project, then create the file
      --  reader from that data.

      Extract_Preprocessor_Data_From_Project
        (Self.Tree.all, Default_Config, File_Configs);
      declare
         FR_Ref : constant File_Reader_Reference :=
           Create_Preprocessor (Default_Config, File_Configs);
         FR_Int : constant Internal_File_Reader_Access :=
           Wrap_Public_File_Reader (FR_Ref);
      begin
         return Wrap_Private_File_Reader (FR_Int);
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ada_file_reader (System.Null_Address);
   end ada_gpr_project_create_preprocessor;

end Libadalang.Implementation.C.Extensions;
