--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces.C.Strings; use Interfaces.C.Strings;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;
with GNATCOLL.Projects;   use GNATCOLL.Projects;
with GNATCOLL.Refcount;   use GNATCOLL.Refcount;
with GNATCOLL.VFS;        use GNATCOLL.VFS;
with GPR2.Containers;
with GPR2.Message;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.View.Set;
with GPR2.Reporter;

with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

with Libadalang.Analysis;          use Libadalang.Analysis;
with Libadalang.Auto_Provider;     use Libadalang.Auto_Provider;
with Libadalang.Config_Pragmas;    use Libadalang.Config_Pragmas;
with Libadalang.GPR_Impl;          use Libadalang.GPR_Impl;
with Libadalang.GPR_Utils;         use Libadalang.GPR_Utils;
with Libadalang.Preprocessing;     use Libadalang.Preprocessing;
with Libadalang.Project_Provider;  use Libadalang.Project_Provider;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Implementation.C.Extensions is

   type GPR2_Tree_Access is access all GPR2.Project.Tree.Object;
   pragma No_Strict_Aliasing (GPR2_Tree_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (GPR2.Project.Tree.Object, GPR2_Tree_Access);
   function Wrap is new Ada.Unchecked_Conversion
     (GPR2_Tree_Access, ada_gpr_project);
   function Unwrap is new Ada.Unchecked_Conversion
     (ada_gpr_project, GPR2_Tree_Access);

   function To_C_Provider
     (Provider : Unit_Provider_Reference) return ada_unit_provider
   is (Wrap_Private_Provider (Wrap_Public_Provider (Provider)));

   function Scenario_Vars_Count
     (Scenario_Vars : System.Address) return Natural;
   --  Return the number of scenario variables in the Scenario_Vars C-style
   --  array. This counts the number of entries before the first NULL entry.

   package String_Vectors is new Ada.Containers.Vectors
     (Positive, Unbounded_String);

   function To_C (Self : String_Vectors.Vector) return ada_string_array_ptr;
   --  Convert a list of strings to the corresponding C value

   procedure Load_Project
     (Project_File                 : chars_ptr;
      Scenario_Vars                : System.Address;
      Target, Runtime, Config_File : chars_ptr;
      Tree                         : out GPR2.Project.Tree.Object;
      Errors                       : out String_Vectors.Vector;
      Exc                          : out Exception_Occurrence);
   --  Helper to load a project file from C arguments. May set Exc to a
   --  ``GNATCOLL.Projects.Invalid_Project`` exception if the project cannot be
   --  loaded: in this case it is up to the caller to re-raise it.
   --  If ``Project_File`` is a null pointer or an empty string, use the
   --  ``GNATCOLL.Projects.Load_Implicit_Project`` function to load the
   --  ``_default.gpr`` project file in the current directory.

   function Fetch_Project
     (Tree         : GPR2.Project.Tree.Object;
      Project_Name : chars_ptr) return GPR2.Project.View.Object;
   --  Helper to fetch the sub-project in ``Tree`` that is called
   --  ``Project_Name``. If that project is unknown, raise a
   --  ``GPR2.Project_Error`` exception. If ``Project_Name`` is null or an
   --  empty string, return ``Undefined``.

   ---------------------------
   -- GPR2 Message Reporter --
   ---------------------------

   package String_Vector_Refs is new GNATCOLL.Refcount.Shared_Pointers
     (String_Vectors.Vector);

   --  Custom GPR2 reporter that intercepts all the messages sent to it so that
   --  we can hand them over the C API.

   type GPR2_Reporter_Type is new GPR2.Reporter.Object with record
      Messages : String_Vector_Refs.Ref;
   end record;

   overriding procedure Internal_Report
     (Self : in out GPR2_Reporter_Type; Message : GPR2.Message.Object);

   overriding function Verbosity
     (Self : GPR2_Reporter_Type) return GPR2.Reporter.Verbosity_Level
   is (GPR2.Reporter.Regular);

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

   ----------
   -- To_C --
   ----------

   function To_C (Self : String_Vectors.Vector) return ada_string_array_ptr is
      I   : int := 1;
   begin
      return Result : constant ada_string_array_ptr :=
        new ada_string_array (int (Self.Length))
      do
         Result.C_Ptr := Result.Items'Address;
         for F of Self loop
            Result.Items (I) :=
              New_String (Ada.Strings.Unbounded.To_String (F));
            I := I + 1;
         end loop;
      end return;
   end To_C;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Project_File                 : chars_ptr;
      Scenario_Vars                : System.Address;
      Target, Runtime, Config_File : chars_ptr;
      Tree                         : out GPR2.Project.Tree.Object;
      Errors                       : out String_Vectors.Vector;
      Exc                          : out Exception_Occurrence)
   is
      Project_File_Value : constant String :=
        (if Project_File = Null_Ptr then "" else Value (Project_File));
      Target_Value       : constant String :=
        (if Target = Null_Ptr then "" else Value (Target));
      Runtime_Value      : constant String :=
        (if Runtime = Null_Ptr then "" else Value (Runtime));
      Config_File_Value  : constant String :=
        (if Config_File = Null_Ptr then "" else Value (Config_File));

      Options : GPR2.Options.Object;
   begin
      if Project_File_Value = "" then
         Options.Add_Switch (GPR2.Options.No_Project);
      else
         Options.Add_Switch (GPR2.Options.P, Project_File_Value);
      end if;

      if Target_Value /= "" then
         Options.Add_Switch (GPR2.Options.Target, Target_Value);
      end if;

      if Runtime_Value /= "" then
         Options.Add_Switch (GPR2.Options.RTS, Runtime_Value);
      end if;

      if Config_File_Value /= "" then
         Options.Add_Switch (GPR2.Options.Config, Config_File_Value);
      end if;

      if Scenario_Vars /= System.Null_Address then
         declare
            Vars : ada_gpr_project_scenario_variable_array
                     (1 .. Scenario_Vars_Count (Scenario_Vars))
               with Import  => True,
                    Address => Scenario_Vars;
         begin
            for V of Vars loop
               declare
                  Var_Name  : constant String := Value (V.Name);
                  Var_Value : constant String := Value (V.Value);
               begin
                  Options.Add_Switch
                    (GPR2.Options.X, Var_Name & "=" & Var_Value);
               end;
            end loop;
         end;
      end if;

      --  Load the project tree and include source information.
      --
      --  Never complain about missing directories: most of the time for
      --  users, this is noise about object directories.

      declare
         Reporter : GPR2_Reporter_Type;
      begin
         begin
            Reporter.Messages.Set (String_Vectors.Empty_Vector);
            if not Tree.Load
              (Options,
               With_Runtime         => True,
               Reporter             => Reporter,
               Artifacts_Info_Level => GPR2.Sources_Units,
               Absent_Dir_Error     => GPR2.No_Error)
            then
               raise GPR2.Project_Error with
                 "fatal error, cannot load the project tree";
            end if;
         exception
            when E : GPR2.Project_Error =>
               Save_Occurrence (Exc, E);
         end;

         --  Forward warnings and errors sent to the GPR2 message reporter
         --  to Errors.

         Errors.Append_Vector (Reporter.Messages.Get);
      end;
   end Load_Project;

   -------------------
   -- Fetch_Project --
   -------------------

   function Fetch_Project
     (Tree         : GPR2.Project.Tree.Object;
      Project_Name : chars_ptr) return GPR2.Project.View.Object
   is
      Name : constant String :=
        (if Project_Name = Null_Ptr then "" else Value (Project_Name));
   begin
      return Result : GPR2.Project.View.Object do
         if Name /= "" then
            Result := Lookup (Tree, Name);
         end if;
      end return;
   end Fetch_Project;

   ------------
   -- Report --
   ------------

   overriding procedure Internal_Report
     (Self : in out GPR2_Reporter_Type; Message : GPR2.Message.Object) is
   begin
      Self.Messages.Get.Append (To_Unbounded_String (Message.Format));
   end Internal_Report;

   ---------------------------
   -- ada_free_string_array --
   ---------------------------

   procedure ada_free_string_array (Strings : ada_string_array_ptr) is
      Value : ada_string_array_ptr := Strings;
   begin
      for F of Value.Items loop
         Free (F);
      end loop;
      Free (Value);
   end ada_free_string_array;

   --------------------------
   -- ada_gpr_project_load --
   --------------------------

   procedure ada_gpr_project_load
     (Project_File                 : chars_ptr;
      Scenario_Vars                : System.Address;
      Target, Runtime, Config_File : chars_ptr;
      Ada_Only                     : int;
      Project                      : access ada_gpr_project;
      Errors                       : access ada_string_array_ptr)
   is
      P   : GPR2_Tree_Access;
      Err : String_Vectors.Vector;
      Exc : Exception_Occurrence;
   begin
      Clear_Last_Exception;

      P := new GPR2.Project.Tree.Object;
      if Ada_Only /= 0 then
         declare
            Langs : GPR2.Containers.Language_Set;
         begin
            Langs.Include (GPR2.Ada_Language);
            P.Restrict_Autoconf_To_Languages (Langs);
         end;
      end if;
      Load_Project
        (Project_File,
         Scenario_Vars,
         Target,
         Runtime,
         Config_File,
         P.all,
         Err,
         Exc);
      if Exception_Identity (Exc) /= Null_Id then

         --  If we have error messages in addition to the exception, extend the
         --  exception message to include them.

         if not Err.Is_Empty then
            declare
               Message : Unbounded_String :=
                 To_Unbounded_String (Exception_Message (Exc));
            begin
               for Error_Message of Err loop
                  Append (Message, ASCII.LF);
                  Append (Message, Error_Message);
               end loop;
               Set_Last_Exception
                 (Exception_Identity (Exc), To_String (Message));
            end;
         else
            Set_Last_Exception (Exc);
         end if;
         Free (P);
         return;
      end if;

      Project.all := Wrap (P);
      Errors.all := To_C (Err);
   end ada_gpr_project_load;

   -----------------------------------
   -- ada_gpr_project_load_implicit --
   -----------------------------------

   procedure ada_gpr_project_load_implicit
     (Target, Runtime, Config_File : chars_ptr;
      Project                      : access ada_gpr_project;
      Errors                       : access ada_string_array_ptr)
   is
   begin
      ada_gpr_project_load
        (Null_Ptr,
         System.Null_Address,
         Target,
         Runtime,
         Config_File,
         0,
         Project,
         Errors);
   end ada_gpr_project_load_implicit;

   --------------------------
   -- ada_gpr_project_free --
   --------------------------

   procedure ada_gpr_project_free (Self : ada_gpr_project) is
      Var_Self : GPR2_Tree_Access := Unwrap (Self);
   begin
      Clear_Last_Exception;
      Free (Var_Self);
   end ada_gpr_project_free;

   ------------------------------------------
   -- ada_gpr_project_create_unit_provider --
   ------------------------------------------

   function ada_gpr_project_create_unit_provider
     (Self    : ada_gpr_project;
      Project : chars_ptr) return ada_unit_provider
   is
      P   : constant GPR2_Tree_Access := Unwrap (Self);
      Prj : GPR2.Project.View.Object;
   begin
      Clear_Last_Exception;

      Prj := Fetch_Project (P.all, Project);
      return To_C_Provider (Create_Project_Unit_Provider (P.all, Prj));
   exception
      when Exc : Unsupported_View_Error | GPR2.Project_Error =>
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
     (Self            : ada_gpr_project;
      Mode            : int;
      Projects_Data   : access chars_ptr;
      Projects_Length : int) return ada_string_array_ptr
   is
      P        : constant GPR2_Tree_Access := Unwrap (Self);
      M        : Source_Files_Mode;
      Projects : GPR2.Project.View.Set.Object;
      Result   : Filename_Vectors.Vector;
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

      --  Decode the ``Projects_Data``/``Projects_Length`` argument

      if Projects_Data /= null then
         declare
            Projects_Array : array (1 .. Natural (Projects_Length))
                             of chars_ptr
            with Import, Address => Projects_Data.all'Address;
         begin
            for I in Projects_Array'Range loop
               Projects.Include
                 (Fetch_Project (P.all, Projects_Array (I)));
            end loop;
         exception
            when Exc : GPR2.Project_Error =>
               Set_Last_Exception (Exc);
               return null;
         end;
      end if;

      --  Compute the list of source files

      Result := Source_Files (P.all, M, Projects);

      --  Convert the vector to the C API result

      declare
         Sources : String_Vectors.Vector;
      begin
         for F of Result loop
            Sources.Append (F);
         end loop;
         return To_C (Sources);
      end;
   end ada_gpr_project_source_files;

   -------------------------------------
   -- ada_gpr_project_default_charset --
   -------------------------------------

   function ada_gpr_project_default_charset
     (Self : ada_gpr_project; Project : chars_ptr) return chars_ptr is
   begin
      Clear_Last_Exception;
      declare
         Tree : GPR2.Project.Tree.Object renames Unwrap (Self).all;
         Prj  : constant GPR2.Project.View.Object :=
           Fetch_Project (Tree, Project);
      begin
         return New_String (Default_Charset_From_Project (Tree, Prj));
      end;
   end ada_gpr_project_default_charset;

   --------------------------------------
   -- ada_create_project_unit_provider --
   --------------------------------------

   function ada_create_project_unit_provider
     (Project_File, Project : chars_ptr;
      Scenario_Vars         : System.Address;
      Target, Runtime       : chars_ptr) return ada_unit_provider
   is
      Tree : GPR2.Project.Tree.Object;
      View : GPR2.Project.View.Object;

      Exc          : Exception_Occurrence;
      Dummy_Errors : String_Vectors.Vector;

      Project_Value : constant String :=
        (if Project = Null_Ptr then "" else Value (Project));
   begin
      Clear_Last_Exception;

      --  Load the project tree

      Load_Project
        (Project_File,
         Scenario_Vars,
         Target,
         Runtime,
         Null_Ptr,
         Tree,
         Dummy_Errors,
         Exc);
      if Exception_Identity (Exc) /= Null_Id then
         Reraise_Occurrence (Exc);
      end if;

      --  Fetch the requested sub-project, if any

      if Project_Value /= "" then
         View := Lookup (Tree, Project_Value);
      end if;

      --  Create the unit provider

      return To_C_Provider (Create_Project_Unit_Provider (Tree, View));

   exception
      when Exc : Unsupported_View_Error | GPR2.Project_Error =>
         Set_Last_Exception (Exc);
         return ada_unit_provider (System.Null_Address);
   end ada_create_project_unit_provider;

   ----------------------------------------
   -- ada_gpr_project_initialize_context --
   ----------------------------------------

   procedure ada_gpr_project_initialize_context
     (Self          : ada_gpr_project;
      Context       : ada_analysis_context;
      Project       : chars_ptr;
      Event_Handler : ada_event_handler;
      With_Trivia   : int;
      Tab_Stop      : int)
   is
      Tree : GPR2.Project.Tree.Object renames Unwrap (Self).all;
      View : GPR2.Project.View.Object;
   begin
      Clear_Last_Exception;

      View := Fetch_Project (Tree, Project);
      Initialize_Context_From_Project
        (Context          => Context,
         Tree             => Tree,
         Project          => View,
         Event_Handler    => Unwrap_Private_Event_Handler (Event_Handler),
         With_Trivia      => With_Trivia /= 0,
         Tab_Stop         => Natural (Tab_Stop));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_gpr_project_initialize_context;

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
     (Self      : ada_gpr_project;
      Project   : chars_ptr;
      Line_Mode : access int) return ada_file_reader
   is
      P              : GPR2.Project.View.Object;
      Default_Config : File_Config;
      File_Configs   : File_Config_Maps.Map;
   begin
      Clear_Last_Exception;

      P := Fetch_Project (Unwrap (Self).all, Project);

      --  Load preprocessor data from the given project, then create the file
      --  reader from that data.

      Extract_Preprocessor_Data_From_Project
        (Unwrap (Self).all, P, Default_Config, File_Configs);

      --  If requested, force the line mode

      if Line_Mode /= null then
         declare
            LM : constant Any_Line_Mode := Any_Line_Mode'Val (Line_Mode.all);

            procedure Force_Line_Mode (Config : in out File_Config);
            --  Callback for Iterate: force the line mode on Config to LM if
            --  preprocessing is enabled for it.

            ---------------------
            -- Force_Line_Mode --
            ---------------------

            procedure Force_Line_Mode (Config : in out File_Config) is
            begin
               if Config.Enabled then
                  Config.Line_Mode := LM;
               end if;
            end Force_Line_Mode;

         begin
            Iterate (Default_Config, File_Configs, Force_Line_Mode'Access);
         end;
      end if;

      --  Create the file reader from that data

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

   ------------------------------------
   -- ada_set_config_pragmas_mapping --
   ------------------------------------

   procedure ada_set_config_pragmas_mapping
     (Context        : ada_analysis_context;
      Global_Pragmas : ada_analysis_unit;
      Local_Pragmas  : access ada_analysis_unit)
   is
   begin
      Clear_Last_Exception;

      declare
         Ctx      : constant Analysis_Context := Wrap_Context (Context);
         Mappings : Config_Pragmas_Mapping;

         type Unit_Array is array (Positive) of ada_analysis_unit;
         LP : Unit_Array with Import, Address => Local_Pragmas.all'Address;
         I  : Positive := 1;
      begin
         Mappings.Global_Pragmas := Wrap_Unit (Global_Pragmas);
         while LP (I) /= null loop
            declare
               Key      : constant Analysis_Unit := Wrap_Unit (LP (I));
               Value    : constant Analysis_Unit := Wrap_Unit (LP (I + 1));
               Dummy    : Libadalang.Config_Pragmas.Unit_Maps.Cursor;
               Inserted : Boolean;
            begin
               Mappings.Local_Pragmas.Insert (Key, Value, Dummy, Inserted);
               if not Inserted then
                  raise Precondition_Failure
                    with "an analysis unit is present twice as a key";
               end if;
               I := I + 2;
            end;
         end loop;

         Set_Mapping (Ctx, Mappings);
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_set_config_pragmas_mapping;

end Libadalang.Implementation.C.Extensions;
