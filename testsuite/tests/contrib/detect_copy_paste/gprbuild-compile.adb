------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2011-2017, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Exceptions;                         use Ada.Exceptions;
with Ada.Strings.Fixed;                      use Ada, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;
with Ada.Text_IO;                            use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT, GNAT.Directory_Operations;
with GNAT.Dynamic_HTables;      use GNAT.Dynamic_HTables;

with Gpr_Build_Util;               use Gpr_Build_Util;
with Gpr_Util;                     use Gpr_Util;
with Gprbuild.Compilation.Process; use Gprbuild.Compilation.Process;
with Gprbuild.Compilation.Slave;
with GPR.Env;
with GPR.Names;                    use GPR.Names;
with GPR.Opt;                      use GPR.Opt;
with GPR.Snames;                   use GPR.Snames;
with GPR.Tempdir;
with GPR.Util;                     use GPR.Util;

package body Gprbuild.Compile is

   procedure Add_Compilation_Switches (Source : Source_Id);
   --  Add to the compilation option, the switches declared in
   --  Compiler'Switches(<source file name>), if it is defined, otherwise in
   --  Compiler'Default_Switches (<language name>), if it is defined.

   procedure Await_Compile
     (Source : out Queue.Source_Info;
      OK     : out Boolean;
      Slave  : out Unbounded_String);
   --  Wait for the end of a compilation and indicate that the object directory
   --  is free.

   procedure Compilation_Phase
     (Main_Project : Project_Id;
      Project_Tree : Project_Tree_Ref);

   procedure Recursive_Import (Project : Project_Id);
   --  Add to table Imports the projects imported by Project, recursively

   function Project_Extends
     (Extending : Project_Id;
      Extended  : Project_Id) return Boolean;
   --  Returns True if Extending is Extended or is extending Extended directly
   --  or indirectly.

   function Directly_Imports
     (Project  : Project_Id;
      Imported : Project_Id) return Boolean;
   --  Returns True if Project directly withs Imported or a project extending
   --  Imported.

   procedure Create_Config_File
     (For_Project  : Project_Id;
      Config       : Language_Config;
      Language     : Name_Id);
   --  Create a new config file

   function Config_File_For
     (Project        : Project_Id;
      Package_Name   : Name_Id;
      Attribute_Name : Name_Id;
      Language       : Name_Id) return Path_Name_Type;
   --  Returns the name of a config file. Returns No_Name if there is no
   --  config file.

   procedure Create_Object_Path_File
     (Project : Project_Id; Shared : Shared_Project_Tree_Data_Access);
   --  Create a temporary file that contains the list of object directories
   --  in the correct order.

   procedure Print_Compilation_Outputs
     (For_Source : Source_Id;
      Always     : Boolean := False);
   --  In complete output mode, or when Always is True, put the outputs from
   --  last compilation to standard output and/or standard error.

   function "<" (Left, Right : Source_Id) return Boolean
     is (Left.File < Right.File);

   package Bad_Compilations_Set is new
     Containers.Indefinite_Ordered_Maps (Source_Id, String);

   Bad_Compilations : Bad_Compilations_Set.Map;
   --  Records bad compilation with the given slave name if any

   Outstanding_Compiles : Natural := 0;
   --  The number of compilation jobs currently spawned

   Slave_Initialized    : Boolean := False;
   --  Record wether the remote compilation slaves have been initialized when
   --  running in distributed mode.

   type Process_Purpose is (Compilation, Dependency);
   --  A type to distinguish between compilation jobs and dependency file
   --  building jobs.

   type Process_Data is record
      Process        : Gprbuild.Compilation.Id  :=
                         Gprbuild.Compilation.Invalid_Process;
      Source         : Queue.Source_Info  := Queue.No_Source_Info;
      Source_Project : Project_Id         := null;
      Mapping_File   : Path_Name_Type     := No_Path;
      Purpose        : Process_Purpose    := Compilation;
      Options        : String_List_Access := null;
   end record;
   --  Data recorded for each spawned jobs, compilation of dependency file
   --  building.

   No_Process_Data : constant Process_Data :=
                       (Process        => Gprbuild.Compilation.Invalid_Process,
                        Source         => Queue.No_Source_Info,
                        Source_Project => null,
                        Mapping_File   => No_Path,
                        Purpose        => Compilation,
                        Options        => null);

   package Compilation_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Gprbuild.Compilation.Process.Header_Num,
      Element    => Process_Data,
      No_Element => No_Process_Data,
      Key        => Gprbuild.Compilation.Id,
      Hash       => Hash,
      Equal      => Gprbuild.Compilation."=");
   --  Hash table to keep data for all spawned jobs

   package Naming_Datas is new GNAT.Table
     (Table_Component_Type => Lang_Naming_Data,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  Naming data when creating config files

   package Imports is new GNAT.HTable.Simple_HTable
     (Header_Num => GPR.Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Project_Id,
      Hash       => Hash,
      Equal      => "=");
   --  When --direct-import-only is used, contains the project ids a non Ada
   --  source is allowed to import source from.

   package Included_Sources is new GNAT.Table
     (Table_Component_Type => Source_Id,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);

   package Subunits is new GNAT.Table
     (Table_Component_Type => GNAT.OS_Lib.String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  A table to store the subunit names when switch --no-split-units is used

   ------------------------------
   -- Add_Compilation_Switches --
   ------------------------------

   Inner_Cargs : aliased String := "-inner-cargs";
   --  When the --compiler-pkg-subst switch is given, this is used to pass
   --  switches from "package Compiler" to the ASIS tool and thence through to
   --  the actual compiler.

   procedure Add_Compilation_Switches (Source : Source_Id) is
      procedure Process_One_Package (Pkg_Name : Name_Id);
      --  Get the switches for the named package

      -------------------------
      -- Process_One_Package --
      -------------------------

      procedure Process_One_Package (Pkg_Name : Name_Id) is
         Options : Variable_Value;
         Ignored : Boolean;
      begin
         Get_Switches
           (Source, Pkg_Name, Project_Tree, Options, Ignored);

         if Options /= Nil_Variable_Value then
            declare
               List            : String_List_Id := Options.Values;
               Element         : String_Element;
               Option          : GNAT.OS_Lib.String_Access;
            begin
               while List /= Nil_String loop
                  Element := Project_Tree.Shared.String_Elements.Table (List);

                  --  Ignore empty options

                  if Element.Value /= Empty_String then
                     Option := Get_Option (Element.Value);

                     Add_Option_Internal_Codepeer
                       (Value       => Option,
                        To          => Compilation_Options,
                        Display     => True);
                  end if;

                  List := Element.Next;
               end loop;
            end;
         end if;
      end Process_One_Package;

   begin
      --  If the --compiler-pkg-subst switch was given, get switches from the
      --  relevant package (e.g. "package Pretty_Printer").

      if Compiler_Pkg_Subst /= No_Name then
         Process_One_Package (Compiler_Pkg_Subst);
         Add_Option_Internal_Codepeer
           (Value       => Inner_Cargs'Unchecked_Access,
            To          => Compilation_Options,
            Display     => True);
      end if;

      --  Always get switches from "package Compiler". If the
      --  --compiler-pkg-subst switch was given, these are preceded by
      --  -inner-cargs (see above) to indicate that the ASIS tool should pass
      --  them along to gcc.

      Process_One_Package (Name_Compiler);
   end Add_Compilation_Switches;

   -------------------
   -- Await_Compile --
   -------------------

   procedure Await_Compile
     (Source : out Queue.Source_Info;
      OK     : out Boolean;
      Slave  : out Unbounded_String)
   is
      use type Gprbuild.Compilation.Id;

      Process   : Gprbuild.Compilation.Id;
      Comp_Data : Process_Data;
      Language  : Language_Ptr;
      Config    : Language_Config;

   begin
      loop
         Source := Queue.No_Source_Info;

         Wait_Result (Process, OK);

         if Process = Gprbuild.Compilation.Invalid_Process then
            return;
         end if;

         Comp_Data := Compilation_Htable.Get (Process);

         if Comp_Data /= No_Process_Data then
            Source := Comp_Data.Source;

            Queue.Set_Obj_Dir_Free (Source.Id.Project.Object_Directory.Name);

            if Comp_Data.Purpose = Compilation then
               Print_Compilation_Outputs
                 (Source.Id, Always => not No_Complete_Output);

               if OK then
                  --  We created a new dependency file, so reset the attributes
                  --  of the old one.

                  Source.Id.Dep_TS := Unknown_Attributes;

                  if Comp_Data.Options /= null
                    and then Source.Id.Switches_Path /= No_Path
                    and then Opt.Check_Switches
                  then
                     --  First, update the time stamp of the object file that
                     --  will be written in the switches file.

                     Source.Id.Object_TS := File_Stamp (Source.Id.Object_Path);

                     --  Write the switches file, now that we have the updated
                     --  time stamp for the object file.

                     declare
                        File : Text_IO.File_Type;

                     begin
                        Create
                          (File,
                           Out_File,
                           Get_Name_String (Source.Id.Switches_Path));

                        Put_Line (File, String (Source.Id.Object_TS));

                        for J in Comp_Data.Options'Range loop
                           Put_Line (File, Comp_Data.Options (J).all);
                        end loop;

                        Close (File);

                     exception
                        when others =>
                           Fail_Program
                             (Source.Tree,
                              "could not create switches file """ &
                              Get_Name_String (Source.Id.Switches_Path) & '"');
                     end;

                     --  For all languages other than Ada, update the time
                     --  stamp of the object file as it is written in the
                     --  global archive dependency file. For all languages,
                     --  update the time stamp of the object file if it is in
                     --  a library project.

                  elsif Source.Id.Language.Config.Dependency_Kind /= ALI_File
                    or else Source.Id.Project.Library
                  then
                     Source.Id.Object_TS := File_Stamp (Source.Id.Object_Path);
                  end if;

               else
                  Set_Failed_Compilation_Status (Comp_Data.Source_Project);

                  Slave := To_Unbounded_String (Get_Slave_For (Process));
               end if;

               Language := Source.Id.Language;

               --  If there is a mapping file used, recycle it in the hash
               --  table of the language.

               if Comp_Data.Mapping_File /= No_Path
                 and then Language /= No_Language_Index
               then
                  Mapping_Files_Htable.Set
                    (T => Language.Mapping_Files,
                     K => Comp_Data.Mapping_File,
                     E => Comp_Data.Mapping_File);
               end if;

               Config := Language.Config;

               if OK
                 and then Config.Dependency_Kind = Makefile
                 and then Config.Compute_Dependency /= No_Name_List
               then
                  declare
                     Current_Dir : constant Dir_Name_Str := Get_Current_Dir;

                     List      : Name_List_Index :=
                                   Config.Compute_Dependency;
                     Nam       : Name_Node :=
                                   Source.Tree.Shared.Name_Lists.Table (List);
                     Exec_Name : constant String :=
                                   Get_Name_String (Nam.Name);
                     Exec_Path : OS_Lib.String_Access;
                  begin
                     Change_Dir
                       (Get_Name_String
                          (Source.Id.Project.Object_Directory.Display_Name));
                     Comp_Data.Mapping_File := No_Path;
                     Comp_Data.Purpose := Dependency;

                     --  ??? We search for it on the PATH for every file,
                     --  this is very inefficient
                     Exec_Path := Locate_Exec_On_Path (Exec_Name);

                     if Exec_Path = null then
                        Fail_Program
                          (Source.Tree,
                           "unable to find dependency builder " & Exec_Name);
                     end if;

                     List := Nam.Next;
                     Compilation_Options.Last := 0;

                     if List = No_Name_List then
                        Name_Len := 0;

                     else
                        loop
                           Nam := Source.Tree.Shared.Name_Lists.Table (List);
                           List := Nam.Next;

                           if List = No_Name_List then
                              Get_Name_String (Nam.Name);
                              exit;
                           end if;

                           Add_Option
                             (Nam.Name, Compilation_Options, Opt.Verbose_Mode);
                        end loop;
                     end if;

                     Add_Str_To_Name_Buffer
                       (Get_Name_String (Source.Id.Path.Display_Name));
                     Add_Option
                       (Name_Buffer (1 .. Name_Len),
                        Compilation_Options,
                        Opt.Verbose_Mode,
                        Simple_Name => not Opt.Verbose_Mode);

                     if not Opt.Quiet_Output then
                        if Opt.Verbose_Mode then
                           Put (Exec_Path.all);
                        else
                           Put (Exec_Name);
                        end if;

                        Put (" ");

                        for Option in 1 .. Compilation_Options.Last loop
                           if Compilation_Options.Visible (Option) then
                              Put
                                (Compilation_Options.Options (Option).all);
                              Put (" ");
                           end if;
                        end loop;

                        New_Line;
                     end if;

                     Comp_Data.Process :=
                       Run
                         (Executable => Exec_Path.all,
                          Options    =>
                            Compilation_Options.Options
                              (1 .. Compilation_Options.Last),
                          Project      => Comp_Data.Source_Project,
                          Obj_Name     => Get_Name_String (Source.Id.Object),
                          Output_File  => Get_Name_String (Source.Id.Dep_Path),
                          Err_To_Out   => True,
                          Force_Local  => True);

                     Compilation_Htable.Set (Comp_Data.Process, Comp_Data);

                     Free (Exec_Path);

                     Change_Dir (Current_Dir);
                  end;

               else
                  Outstanding_Compiles := Outstanding_Compiles - 1;
                  return;
               end if;

            elsif Comp_Data.Purpose = Dependency then
               Outstanding_Compiles := Outstanding_Compiles - 1;
               return;
            end if;
         end if;
      end loop;
   end Await_Compile;

   ---------------------
   -- Config_File_For --
   ---------------------

   function Config_File_For
     (Project        : Project_Id;
      Package_Name   : Name_Id;
      Attribute_Name : Name_Id;
      Language       : Name_Id) return Path_Name_Type
   is
      function Absolute_Path
        (Path    : Path_Name_Type;
         Project : Project_Id) return Path_Name_Type;
      --  Returns an absolute path for a config file

      -------------------
      -- Absolute_Path --
      -------------------

      function Absolute_Path
        (Path    : Path_Name_Type;
         Project : Project_Id) return Path_Name_Type
      is
      begin
         Get_Name_String (Path);

         if not Is_Absolute_Path (Name_Buffer (1 .. Name_Len)) then
            Get_Name_String (Project.Directory.Display_Name);
            if Name_Buffer (Name_Len) /= Directory_Separator then
               Add_Char_To_Name_Buffer (Directory_Separator);
            end if;

            Add_Str_To_Name_Buffer (Get_Name_String (Path));
         end if;

         return Name_Find;
      end Absolute_Path;

      Config_Package  : constant Package_Id :=
                          Value_Of
                            (Name        => Package_Name,
                             In_Packages => Project.Decl.Packages,
                             Shared      => Project_Tree.Shared);

      Config_Variable : Variable_Value :=
                          Value_Of
                            (Name                    => Language,
                             Attribute_Or_Array_Name => Attribute_Name,
                             In_Package              => Config_Package,
                             Shared                  => Project_Tree.Shared);

   begin
      --  Get the config pragma attribute when the language is Ada and the
      --  config file attribute is not declared.

      if Config_Variable = Nil_Variable_Value
        and then Config_Package /= No_Package
        and then Language = Name_Ada
      then
         if Attribute_Name = Name_Global_Config_File then
            Config_Variable :=
              Value_Of
                (Variable_Name => Name_Global_Configuration_Pragmas,
                 In_Variables  => Project_Tree.Shared.Packages.Table
                   (Config_Package).Decl.Attributes,
                 Shared        => Project_Tree.Shared);

         elsif Attribute_Name = Name_Local_Config_File then
            Config_Variable :=
              Value_Of
                (Variable_Name => Name_Local_Configuration_Pragmas,
                 In_Variables  => Project_Tree.Shared.Packages.Table
                   (Config_Package).Decl.Attributes,
                 Shared        => Project_Tree.Shared);
         end if;
      end if;

      if Config_Variable = Nil_Variable_Value then
         return No_Path;

      else
         Get_Name_String (Config_Variable.Value);

         if Name_Len = 0 then
            return No_Path;

         else
            return
              Absolute_Path
                (Path_Name_Type (Config_Variable.Value),
                 Config_Variable.Project);
         end if;
      end if;
   end Config_File_For;

   ------------------------
   -- Create_Config_File --
   ------------------------

   procedure Create_Config_File
     (For_Project : Project_Id;
      Config      : Language_Config;
      Language    : Name_Id)
   is

      File_Name : Path_Name_Type  := No_Path;
      File      : File_Descriptor := Invalid_FD;

      Source   : Source_Id;
      Iter     : Source_Iterator;

      procedure Check
        (Project : Project_Id;
         Tree    : Project_Tree_Ref;
         Dummy   : in out Boolean);
      --  Check the naming schemes of the different projects of the project
      --  tree. For each different naming scheme issue the pattern config
      --  declarations.

      procedure Check_Temp_File;
      --  Check if a temp file has been created. If not, create one

      procedure Copy_Config_File
        (Project        : Project_Id;
         Package_Name   : Name_Id;
         Attribute_Name : Name_Id;
         Language       : Name_Id);
      --  If a specified config file exists, copy it in the temporary config
      --  file.

      procedure Put_Line (File : File_Descriptor; S : String);
      --  Output procedure, analogous to normal Text_IO proc of same name

      -----------
      -- Check --
      -----------

      procedure Check
        (Project : Project_Id;
         Tree    : Project_Tree_Ref;
         Dummy   : in out Boolean)
      is
         pragma Unreferenced (Dummy, Tree);
         Lang_Id : Language_Ptr := Project.Languages;

         Current_Naming : Positive := 1;

         procedure Replace;

         -------------
         -- Replace --
         -------------

         procedure Replace is
            Cur : Positive := 1;

            procedure Substitute (N : File_Name_Type);
            procedure Substitute (Name : String);

            ----------------
            -- Substitute --
            ----------------

            procedure Substitute (N : File_Name_Type) is
            begin
               if N = No_File then
                  Cur := Cur + 1;

               else
                  Substitute (Get_Name_String (N));
               end if;
            end Substitute;

            procedure Substitute (Name : String) is
            begin
               Name_Buffer
                 (Cur + Name'Length .. Name_Len - 2 + Name'Length) :=
                 Name_Buffer (Cur + 2 .. Name_Len);
               Name_Buffer (Cur .. Cur + Name'Length - 1) := Name;
               Name_Len := Name_Len - 2 + Name'Length;
               Cur := Cur + Name'Length;
            end Substitute;

         begin
            while Cur < Name_Len loop
               if Name_Buffer (Cur) = '%' then
                  case Name_Buffer (Cur + 1) is
                     when 'b' =>
                        Substitute (Lang_Id.Config.Naming_Data.Body_Suffix);

                     when 's' =>
                        Substitute (Lang_Id.Config.Naming_Data.Spec_Suffix);

                     when 'd' =>
                        Substitute
                          (Lang_Id.Config.Naming_Data.Dot_Replacement);

                     when 'c' =>
                        Substitute
                          (Image (Lang_Id.Config.Naming_Data.Casing));

                     when '%' =>
                        Name_Buffer (Cur .. Name_Len - 1) :=
                          Name_Buffer (Cur + 1 .. Name_Len);
                        Name_Len := Name_Len - 1;
                        Cur := Cur + 1;

                     when others =>
                        Cur := Cur + 1;
                  end case;

               else
                  Cur := Cur + 1;
               end if;
            end loop;
         end Replace;

      begin
         if Current_Verbosity = High then
            Put ("Checking project file """);
            Put (Get_Name_String (Project.Name));
            Put (""".");
            New_Line;
         end if;

         while Lang_Id /= No_Language_Index loop
            exit when Lang_Id.Name = Language;
            Lang_Id := Lang_Id.Next;
         end loop;

         if Lang_Id /= No_Language_Index then
            Current_Naming := Naming_Datas.First;

            while Current_Naming <= Naming_Datas.Last loop
               exit when Naming_Datas.Table (Current_Naming) =
                 Lang_Id.Config.Naming_Data;
               Current_Naming := Current_Naming + 1;
            end loop;

            if Current_Naming > Naming_Datas.Last then
               Naming_Datas.Increment_Last;
               Naming_Datas.Table (Naming_Datas.Last) :=
                 Lang_Id.Config.Naming_Data;

               Check_Temp_File;

               if Lang_Id.Config.Config_Spec_Pattern /= No_Name then
                  Get_Name_String (Lang_Id.Config.Config_Spec_Pattern);
                  Replace;
                  Put_Line (File, Name_Buffer (1 .. Name_Len));
               end if;

               if Lang_Id.Config.Config_Body_Pattern /= No_Name then
                  Get_Name_String (Lang_Id.Config.Config_Body_Pattern);
                  Replace;
                  Put_Line (File, Name_Buffer (1 .. Name_Len));
               end if;
            end if;
         end if;
      end Check;

      ---------------------
      -- Check_Temp_File --
      ---------------------

      procedure Check_Temp_File is
      begin
         if File = Invalid_FD then
            Tempdir.Create_Temp_File (File, Name => File_Name);

            if File = Invalid_FD then
               Fail_Program
                 (Project_Tree,
                  "unable to create temporary configuration pragmas file");

            else
               Record_Temp_File (Project_Tree.Shared, File_Name);

               if Opt.Verbosity_Level > Opt.Low then
                  Put ("Creating temp file """);
                  Put (Get_Name_String (File_Name));
                  Put_Line ("""");
               end if;
            end if;
         end if;
      end Check_Temp_File;

      ----------------------
      -- Copy_Config_File --
      ----------------------

      procedure Copy_Config_File
        (Project        : Project_Id;
         Package_Name   : Name_Id;
         Attribute_Name : Name_Id;
         Language       : Name_Id)
      is
         Config_File_Path : constant Path_Name_Type :=
                              Config_File_For
                                (Project, Package_Name,
                                 Attribute_Name, Language);
         Config_File      : Text_IO.File_Type;
         Line             : String (1 .. 1_000);
         Last             : Natural;

      begin
         if Config_File_Path /= No_Path then
            begin
               Open (Config_File, In_File, Get_Name_String (Config_File_Path));

            exception
               when others =>
                  Fail_Program
                    (Project_Tree,
                     "unable to open config file "
                     & Get_Name_String (Config_File_Path));
            end;

            Check_Temp_File;

            while not End_Of_File (Config_File) loop
               Get_Line (Config_File, Line, Last);
               Put_Line (File, Line (1 .. Last));
            end loop;

            Close (Config_File);
         end if;
      end Copy_Config_File;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (File : File_Descriptor; S : String) is
         S0   : String (1 .. S'Length + 1);
         Last : Natural;

      begin
         --  Add an ASCII.LF to the string. As this config file is supposed to
         --  be used only by the compiler, we don't care about the characters
         --  for the end of line. In fact we could have put a space, but
         --  it is more convenient to be able to read gnat.adc during
         --  development, for which the ASCII.LF is fine.

         S0 (1 .. S'Length) := S;
         S0 (S0'Last) := ASCII.LF;
         Last := Write (File, S0'Address, S0'Length);

         if Last /= S'Length + 1 then
            Fail_Program (Project_Tree, "Disk full");
         end if;

         if Current_Verbosity = High then
            Put_Line (S);
         end if;
      end Put_Line;

      procedure Check_All_Projects is
        new For_Every_Project_Imported (Boolean, Check);
      Dummy : Boolean := False;

      --  Start of processing for Create_Config_File

   begin
      --  Nothing to do if config has already been checked

      if For_Project.Config_Checked then
         return;
      end if;

      if Config.Config_File_Unique then
         --  Copy an eventual global config file

         Copy_Config_File
           (Main_Project, Name_Builder, Name_Global_Config_File, Language);

         --  Copy an eventual local config file

         Copy_Config_File
           (For_Project, Name_Compiler, Name_Local_Config_File, Language);
      end if;

      For_Project.Config_Checked := True;

      Naming_Datas.Init;

      Check_All_Projects (For_Project, Project_Tree, Dummy);

      --  Visit all the units and issue the config declarations for those that
      --  need one.

      Iter := For_Each_Source (Project_Tree);

      loop
         Source := GPR.Element (Iter);
         exit when Source = No_Source;

         if Source.Language.Name = Language
           and then Source.Naming_Exception /= No
           and then Source.Unit /= No_Unit_Index
           and then not Source.Locally_Removed
           and then Source.Replaced_By = No_Source
         then
            Name_Len := 0;

            if Source.Kind = Spec then
               if Source.Index = 0 and then Config.Config_Spec /= No_Name then
                  Get_Name_String (Config.Config_Spec);

               elsif
                 Source.Index /= 0 and then Config.Config_Spec_Index /= No_Name
               then
                  Get_Name_String (Config.Config_Spec_Index);
               end if;

            else
               if Source.Index = 0 and then Config.Config_Body /= No_Name then
                  Get_Name_String (Config.Config_Body);

               elsif
                 Source.Index /= 0 and then Config.Config_Body_Index /= No_Name
               then
                  Get_Name_String (Config.Config_Body_Index);
               end if;
            end if;

            if Name_Len /= 0 then
               declare
                  Cur       : Positive := 1;
                  Unit      : constant String :=
                                Get_Name_String (Source.Unit.Name);
                  File_Name : constant String :=
                                Get_Name_String (Source.Display_File);

               begin
                  while Cur < Name_Len loop
                     if Name_Buffer (Cur) /= '%' then
                        Cur := Cur + 1;

                     else
                        case Name_Buffer (Cur + 1) is
                           when 'u' =>
                              Name_Buffer
                                (Cur + Unit'Length ..
                                   Name_Len - 2 + Unit'Length) :=
                                  Name_Buffer (Cur + 2 .. Name_Len);
                              Name_Buffer (Cur .. Cur + Unit'Length - 1) :=
                                Unit;
                              Cur := Cur + Unit'Length;
                              Name_Len := Name_Len - 2 + Unit'Length;

                           when 'f' =>
                              Name_Buffer
                                (Cur + File_Name'Length ..
                                   Name_Len - 2 + File_Name'Length) :=
                                  Name_Buffer (Cur + 2 .. Name_Len);
                              Name_Buffer
                                (Cur .. Cur + File_Name'Length - 1) :=
                                File_Name;
                              Cur := Cur + File_Name'Length;
                              Name_Len := Name_Len - 2 + File_Name'Length;

                           when 'i' =>
                              declare
                                 Index_String : constant String :=
                                   Source.Index'Img;

                              begin
                                 Name_Buffer
                                   (Cur + Index_String'Length ..
                                      Name_Len - 2 + Index_String'Length) :=
                                     Name_Buffer (Cur + 2 .. Name_Len);
                                 Name_Buffer
                                   (Cur .. Cur + Index_String'Length - 1) :=
                                   Index_String;
                                 Cur := Cur + Index_String'Length;
                                 Name_Len :=
                                   Name_Len - 2 + Index_String'Length;
                              end;

                           when '%' =>
                              Name_Buffer (Cur .. Name_Len - 1) :=
                                Name_Buffer (Cur + 1 .. Name_Len);
                              Cur := Cur + 1;
                              Name_Len := Name_Len - 1;

                           when others =>
                              Cur := Cur + 1;
                        end case;
                     end if;
                  end loop;

                  Put_Line (File, Name_Buffer (1 .. Name_Len));
               end;
            end if;
         end if;

         Next (Iter);
      end loop;

      if File /= Invalid_FD then
         Close (File);
         For_Project.Config_File_Name := File_Name;
      end if;
   end Create_Config_File;

   -----------------------------
   -- Create_Object_Path_File --
   -----------------------------

   procedure Create_Object_Path_File
     (Project : Project_Id; Shared : Shared_Project_Tree_Data_Access)
   is
      FD   : File_Descriptor;
      Name : Path_Name_Type;

      LF : constant String := (1 => ASCII.LF);

      procedure Add
        (Project : Project_Id;
         In_Tree : Project_Tree_Ref;
         Dummy   : in out Boolean);
      --  Add the object directory of a project to the file

      ---------
      -- Add --
      ---------

      procedure Add
        (Project : Project_Id;
         In_Tree : Project_Tree_Ref;
         Dummy   : in out Boolean)
      is
         pragma Unreferenced (Dummy, In_Tree);

         Path : constant Path_Name_Type :=
                  Get_Object_Directory
                    (Project,
                     Including_Libraries => True,
                     Only_If_Ada         => False);

         Last : Natural;

         pragma Unreferenced (Last);

      begin
         if Path /= No_Path then
            Get_Name_String (Path);
            Last := Write (FD, Name_Buffer (1)'Address, Name_Len);
            Last := Write (FD, LF (1)'Address, 1);
         end if;

         Dummy := True;
      end Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Add);

      Status : Boolean := False;
      pragma Warnings (Off, Status);

   begin
      GPR.Env.Create_Temp_File (Shared, FD, Name, "object path file");
      Project.Object_Path_File := Name;

      For_All_Projects
        (Project, Project_Tree, Status, Include_Aggregated => True);

      Close (FD, Status);
   end Create_Object_Path_File;

   ----------------------
   -- Recursive_Import --
   ----------------------

   procedure Recursive_Import (Project : Project_Id) is
      Ext : constant Project_Id := Project.Extends;
      L   : Project_List := Project.Imported_Projects;

   begin
      if Ext /= No_Project
        and then not Imports.Get (Ext)
      then
         Imports.Set (Ext, True);
         Recursive_Import (Ext);
      end if;

      while L /= null loop
         if not Imports.Get (L.Project) then
            Imports.Set (L.Project, True);
            Recursive_Import (L.Project);
         end if;

         L := L.Next;
      end loop;
   end Recursive_Import;

   ----------------------
   -- Directly_Imports --
   ----------------------

   function Directly_Imports
     (Project  : Project_Id;
      Imported : Project_Id) return Boolean
   is
      L : Project_List := Project.Imported_Projects;
      P : Project_Id;

   begin
      while L /= null loop
         P := L.Project;
         while P /= No_Project loop
            if Imported = P then
               return True;
            end if;

            P := P.Extends;
         end loop;

         L := L.Next;
      end loop;

      return False;
   end Directly_Imports;

   -------------------------------
   -- Print_Compilation_Outputs --
   -------------------------------

   procedure Print_Compilation_Outputs
     (For_Source : Source_Id;
      Always     : Boolean := False) is
   begin
      if Complete_Output or else Always then
         declare
            Proj : constant Project_Id :=
              Ultimate_Extending_Project_Of (For_Source.Project);
            File_Path : constant String :=
              Get_Name_String (Proj.Object_Directory.Name) &
              Directory_Separator &
              Get_Name_String (For_Source.File);

            File : Ada.Text_IO.File_Type;
            Line : String (1 .. 1_000);
            Last : Natural;
         begin
            begin
               Open (File, In_File, File_Path & ".stdout");

               while not End_Of_File (File) loop
                  Get_Line (File, Line, Last);
                  Put_Line (Standard_Output, Line (1 .. Last));
               end loop;

               Close (File);

               Open (File, In_File, File_Path & ".stderr");

               while not End_Of_File (File) loop
                  Get_Line (File, Line, Last);
                  Put_Line (Standard_Error, Line (1 .. Last));
               end loop;

               Close (File);
            exception
                  --  Ignore any exception. Most likely it means that the
                  --  files do no exist.

               when others => null;
            end;
         end;
      end if;
   end Print_Compilation_Outputs;

   ---------
   -- Run --
   ---------

   procedure Run is

      procedure Do_Compile (Project : Project_Id; Tree : Project_Tree_Ref);

      ----------------
      -- Do_Compile --
      ----------------

      procedure Do_Compile (Project : Project_Id; Tree : Project_Tree_Ref) is
         use type Containers.Count_Type;
      begin
         if Builder_Data (Tree).Need_Compilation then
            Compilation_Phase (Project, Tree);

            if Total_Errors_Detected > 0
              or else not Bad_Compilations.Is_Empty
            then
               --  If switch -k or -jnn (with nn > 1), output a summary of the
               --  sources that could not be compiled.

               if (Opt.Keep_Going or else Get_Maximum_Processes > 1)
                 and then not Bad_Compilations.Is_Empty
                 and then not Opt.No_Exit_Message
               then
                  New_Line;

                  for Index in Bad_Compilations.Iterate loop
                     declare
                        Source : constant Source_Id :=
                                   Bad_Compilations_Set.Key (Index);
                        Slave  : constant String :=
                                   Bad_Compilations_Set.Element (Index);
                     begin
                        if Source /= No_Source then
                           Put ("   compilation of ");
                           Put
                             (Get_Name_String (Source.Display_File));
                           Put (" failed");
                           if Slave /= "" then
                              Put (" on " & Slave);
                           end if;
                           New_Line;
                        end if;
                     end;
                  end loop;

                  New_Line;
               end if;

               if Opt.Keep_Going and then Project.Qualifier = Aggregate then
                  Bad_Compilations.Clear;
                  Exit_Code := E_Fatal;

               else
                  if Distributed_Mode and then Slave_Initialized then
                     Gprbuild.Compilation.Slave.Unregister_Remote_Slaves;
                  end if;

                  Fail_Program
                    (Tree, "*** compilation phase failed",
                     No_Message => Opt.No_Exit_Message);
               end if;
            end if;
         end if;
      end Do_Compile;

      procedure Compile_All is new For_Project_And_Aggregated (Do_Compile);

   begin
      Compile_All (Main_Project, Project_Tree);

      --  Unregister the slaves and get back compiled object code. This is a
      --  nop if no compilation has been done.

      Gprbuild.Compilation.Slave.Unregister_Remote_Slaves;
   end Run;

   -----------------------
   -- Compilation_Phase --
   -----------------------

   procedure Compilation_Phase
     (Main_Project : Project_Id;
      Project_Tree : Project_Tree_Ref)
   is
      type Local_Project_Data is record
         Include_Language       : Language_Ptr := No_Language_Index;
         --  Prepared arguments for "include" parameters (-I or include file).
         --  These are specific to each language and project.

         Include_Path_File      : Path_Name_Type;
         --  The path name of the of the source search directory file

         Imported_Dirs_Switches : Argument_List_Access;
         --  List of the source search switches (-I<source dir>) to be used
         --  when compiling.

         Include_Path           : OS_Lib.String_Access := null;
         --  The search source path for the project. Used as the value for an
         --  environment variable, specified by attribute Include_Path
         --  (<langu>). The names of the environment variables are in component
         --  Include_Path of the records Language_Config.
      end record;
      --  project-specific data required for this procedure. These are not
      --  stored in the Project_Data record so that projects kept in memory do
      --  not have to allocate space for these temporary data

      No_Local_Project_Data : constant Local_Project_Data :=
                                (Include_Language       => No_Language_Index,
                                 Include_Path           => null,
                                 Imported_Dirs_Switches => null,
                                 Include_Path_File      => No_Path);

      package Local_Projects_HT is new Simple_HTable
        (Header_Num => GPR.Header_Num,
         Element    => Local_Project_Data,
         No_Element => No_Local_Project_Data,
         Key        => Project_Id,
         Hash       => GPR.Hash,
         Equal      => "=");

      Local_Projects : Local_Projects_HT.Instance;

      Current_Project      : Project_Id := No_Project;
      Current_Language_Ind : Language_Ptr := No_Language_Index;
      --  The project for which the include path environment has been set last,
      --  to avoid computing it several times.

      Dep_File : Text_File;
      Start    : Natural;
      Finish   : Natural;
      Last_Obj : Natural;

      procedure Add_Config_File_Switch
        (Config    : Language_Config;
         Path_Name : Path_Name_Type);

      procedure Record_ALI_For
        (Source_Identity : Queue.Source_Info;
         The_ALI         : ALI.ALI_Id := ALI.No_ALI_Id);
      --  Record the Id of an ALI file in Good_ALI table.
      --  The_ALI can contain the pre-parsed ali file, to save time.
      --  Tree is the tree to which Source_Identity belongs

      function Phase_2_Makefile (Src_Data : Queue.Source_Info) return Boolean;
      function Phase_2_ALI (Src_Data : Queue.Source_Info) return Boolean;
      --  Process Wait_For_Available_Slot depending on Src_Data.Dependency type
      --  This returns whether the compilation is considered as successful or
      --  not.

      procedure Set_Options_For_File (Id : Source_Id);
      --  Prepare the compiler options to use when building Id

      procedure Process_Project_Phase_1 (Source : Queue.Source_Info);
      --  If some compilation is needed for this project, perform it

      function Must_Exit_Because_Of_Error return Boolean;
      --  Return True if there were errors and the user decided to exit in such
      --  a case. This waits for any outstanding compilation.

      function Check_Switches_File (Id : Source_Id) return Boolean;
      --  Check in its switches file where Id was compiled with the same
      --  switches

      procedure Update_Object_Path
        (Id : Source_Id; Source_Project : Project_Id);
      --  Update, if necessary, the path of the object file, of the dependency
      --  file and of the switches file, in the case of the compilation of a
      --  source in an extended project, when the source is in a project being
      --  extended.

      procedure Add_Dependency_Options (Id : Source_Id);
      --  Add switches to the compilation command line to create the
      --  dependency file

      procedure Add_Object_File_Switches (Id : Source_Id);
      --  If there are switches to specify the name of the object file, add
      --  them.

      procedure Add_Object_Path_Switches (Id : Source_Id);
      --  If attribute Compiler'Object_Path_Switches has been specified, create
      --  the temporary object path file, if not already done, and add the
      --  switch(es) to the invocation of the compiler.

      procedure Add_Config_File_Switches
        (Id             : Source_Id;
         Source_Project : Project_Id);
      --  If Config_File_Switches is specified, check if a config file need to
      --  be specified. Return the path to the config file

      procedure Add_Trailing_Switches (Id : Source_Id);
      --  Add the trailing required switches, if any, so that they will be put
      --  in the switches file.

      procedure Add_Name_Of_Source_Switches (Id : Source_Id);
      --  Add the name of the source to be compiled

      function Add_Mapping_File_Switches
        (Source         : Queue.Source_Info;
         Source_Project : Project_Id) return Path_Name_Type;
      --  If the compiler supports mapping files, add the necessary switch.
      --  Returns the name of the mapping file to use (or No_File)

      procedure Add_Multi_Unit_Switches (Id : Source_Id);
      --  Add, if needed, the required switches to compile a multi-unit source
      --  file.

      procedure Spawn_Compiler_And_Register
        (Source                 : Queue.Source_Info;
         Source_Project         : Project_Id;
         Compiler_Path          : String;
         Mapping_File_Path      : Path_Name_Type;
         Last_Switches_For_File : Integer);
      --  Spawn the compiler with the arguments currently set in
      --  Compiler_Options. It registers the process we just spawned, so that
      --  we start monitoring it.
      --  This also displays on the output the command we are spawning.
      --  Last_Switches_For_File is the index in Compilation_Options of the
      --  last switch that should be written to the switches file. All
      --  following switches are not output in that file.

      function Get_Compatible_Languages (Lang : Language_Ptr) return Name_Ids;
      --  Return the list of languages that Id could potentially include (for
      --  instance "C" if Id is a "C++" file. This also includes Id's own
      --  language.

      procedure Prepare_Imported_Dirs_Switches
        (Data    : out Local_Project_Data;
         Project : Project_Id;
         Lang    : Language_Ptr);
      --  Add the switches for include directories to the command line (these
      --  are the "-I" switches in the case of C for instance).

      procedure Prepare_Include_Path_File
        (Data    : out Local_Project_Data;
         Project : Project_Id;
         Lang    : Language_Ptr);
      --  Create a file to pass the include directories to the compiler

      procedure Start_Compile_If_Possible;
      --  Checks if there is more work that we can do (ie the Queue is non
      --  empty). If there is, do it only if we have not yet used up all the
      --  available processes.

      procedure Wait_For_Available_Slot;
      --  Check if we should wait for a compilation to finish. This is the case
      --  if all the available processes are busy compiling sources or there is
      --  nothing else to do (that is the Q is empty and there are outstanding
      --  compilations).

      procedure Set_Env_For_Include_Dirs
        (Id : Source_Id;  Source_Project : Project_Id);
      --  Set environment variables or switches to pass the include directories
      --  to the compiler

      ----------------------------
      -- Add_Config_File_Switch --
      ----------------------------

      procedure Add_Config_File_Switch
        (Config    : Language_Config;
         Path_Name : Path_Name_Type)
      is
         List : Name_List_Index := Config.Config_File_Switches;
         Nam  : Name_Node;

      begin
         while List /= No_Name_List loop
            Nam := Project_Tree.Shared.Name_Lists.Table (List);
            Get_Name_String (Nam.Name);

            if Nam.Next = No_Name_List then
               Add_Str_To_Name_Buffer (Get_Name_String (Path_Name));
            end if;

            Add_Option
              (Name_Buffer (1 .. Name_Len),
               To      => Compilation_Options,
               Display => Opt.Verbose_Mode);

            List := Nam.Next;
         end loop;
      end Add_Config_File_Switch;

      --------------------
      -- Record_ALI_For --
      --------------------

      procedure Record_ALI_For
        (Source_Identity : Queue.Source_Info;
         The_ALI         : ALI.ALI_Id := ALI.No_ALI_Id)
      is
         Local_ALI : ALI.ALI_Id := The_ALI;
         Text      : Text_Buffer_Ptr;

      begin
         if The_ALI = ALI.No_ALI_Id then
            Text := Read_Library_Info_From_Full
              (File_Name_Type (Source_Identity.Id.Dep_Path),
               Source_Identity.Id.Dep_TS'Access);

            if Text /= null then
               --  Read the ALI file but read only the necessary lines

               Local_ALI :=
                 ALI.Scan_ALI
                   (File_Name_Type (Source_Identity.Id.Dep_Path),
                    Text,
                    Ignore_ED     => False,
                    Err           => True,
                    Read_Lines    => "W");
               Free (Text);
            end if;
         end if;

         if Local_ALI /= ALI.No_ALI_Id then
            Queue.Insert_Withed_Sources_For (Local_ALI, Source_Identity.Tree);

            ALI.Initialize_ALI;
            --  ALI.Util.Initialize_ALI_Source;
         end if;
      end Record_ALI_For;

      ----------------------
      -- Phase_2_Makefile --
      ----------------------

      function Phase_2_Makefile
        (Src_Data : Queue.Source_Info) return Boolean is

         Object_Path : GNAT.OS_Lib.String_Access;

         type Src_Record;
         type Src_Access is access Src_Record;
         type Src_Record is record
            File : GNAT.OS_Lib.String_Access;
            TS   : Time_Stamp_Type;
            Next : Src_Access;
         end record;

         First_Src : Src_Access := null;
         Last_Src  : Src_Access := null;

         procedure Free is new Unchecked_Deallocation (Src_Record, Src_Access);

         procedure Purge;
         --  Deallocate Object_Path and the list of sources rooted at
         --  First_Src.

         -----------
         -- Purge --
         -----------

         procedure Purge is
            Curr : Src_Access := First_Src;
            Next : Src_Access := null;

         begin
            Free (Object_Path);

            Curr := First_Src;
            First_Src := null;
            Last_Src  := null;

            while Curr /= null loop
               Next := Curr.Next;
               Free (Curr.File);
               Free (Curr);
               Curr := Next;
            end loop;
         end Purge;

         Compilation_OK  : Boolean := True;
         Dep_File_OK     : Boolean := False;
      begin
         Open (Dep_File, Get_Name_String (Src_Data.Id.Dep_Path));

         if Is_Valid (Dep_File) then

            Big_Loop :
            loop
               declare
                  End_Of_File_Reached : Boolean := False;
                  Object_Found        : Boolean := False;
               begin
                  loop
                     if End_Of_File (Dep_File) then
                        End_Of_File_Reached := True;
                        exit;
                     end if;

                     Get_Line (Dep_File, Name_Buffer, Name_Len);

                     if Name_Len > 0
                       and then Name_Buffer (1) /= '#'
                     then
                        --  Skip a first line that is an empty
                        --  continuation line.

                        for J in 1 .. Name_Len - 1 loop
                           if Name_Buffer (J) /= ' ' then
                              Object_Found := True;
                              exit;
                           end if;
                        end loop;

                        exit when Object_Found
                          or else Name_Buffer (Name_Len) /= '\';
                     end if;
                  end loop;

                  exit Big_Loop when End_Of_File_Reached;
               end;

               Start  := 1;
               Finish := Index (Name_Buffer (1 .. Name_Len), ": ");

               exit Big_Loop when Finish = 0;

               Last_Obj := Finish;
               loop
                  Last_Obj := Last_Obj - 1;
                  exit when Last_Obj = Start
                    or else Name_Buffer (Last_Obj) /= ' ';
               end loop;

               while Start < Last_Obj
                 and then Name_Buffer (Start) = ' '
               loop
                  Start := Start + 1;
               end loop;

               Object_Path := new String'(Name_Buffer (Start .. Last_Obj));
               Dep_File_OK := True;

               Start := Finish + 2;

               --  Process each line

               Line_Loop : loop
                  declare
                     Line : constant String  := Name_Buffer (1 .. Name_Len);
                     Last : constant Natural := Name_Len;

                  begin
                     Name_Loop : loop

                        --  Find the beginning of the next source path
                        --  name.

                        while Start < Last and then Line (Start) = ' ' loop
                           Start := Start + 1;
                        end loop;

                        --  Go to next line when there is a
                        --  continuation character \ at the end of the
                        --  line.

                        exit Name_Loop when Start = Last
                          and then Line (Start) = '\';

                        --  We should not be at the end of the line,
                        --  without a continuation character \.

                        exit Name_Loop when Start = Last;

                        --  Look for the end of the source path name

                        Finish := Start;

                        while Finish < Last loop
                           if Line (Finish) = '\' then
                              --  On Windows, a '\' is part of the
                              --  path name, except when it is not the
                              --  first character followed by another
                              --  '\' or by a space. On other
                              --  platforms, when we are getting a '\'
                              --  that is not the last character of
                              --  the line, the next character is part
                              --  of the path name, even if it is a
                              --  space.

                              if On_Windows
                                and then Finish = Start
                                and then Line (Finish + 1) = '\'
                              then
                                 Finish := Finish + 2;

                              else
                                 Finish := Finish + 1;
                              end if;

                           else
                              --  A space that is not preceded by '\'
                              --  indicates the end of the path name.

                              exit when Line (Finish + 1) = ' ';
                              Finish := Finish + 1;
                           end if;
                        end loop;

                        --  Check this source

                        declare
                           Src_Name : constant String :=
                                        Normalize_Pathname
                                         (Name           =>
                                            Line (Start .. Finish),
                                          Resolve_Links  => False,
                                          Case_Sensitive => False);
                           Src_Name_Id : Name_Id;

                           Unescaped : constant String :=
                                     Unescape (Line (Start .. Finish));
                           Unescaped_Id : Name_Id;
                           --  Only use to get the time stamp

                           Source_2   : Source_Id;
                           Src_TS     : Time_Stamp_Type;

                        begin
                           Name_Len := 0;
                           Add_Str_To_Name_Buffer (Src_Name);
                           Src_Name_Id := Name_Find;
                           Name_Len := 0;
                           Add_Str_To_Name_Buffer (Unescaped);
                           Unescaped_Id := Name_Find;

                           Source_2 := Source_Paths_Htable.Get
                             (Src_Data.Tree.Source_Paths_HT,
                              Path_Name_Type (Src_Name_Id));
                           Src_TS :=
                             File_Stamp (File_Name_Type (Unescaped_Id));

                           if Source_2 /= No_Source then
                              --  It is a source of a project

                              if not Project_Extends
                                (Src_Data.Id.Project, Source_2.Project)
                                and then
                                  not Project_Extends
                                    (Source_2.Project, Src_Data.Id.Project)
                              then
                                 --  It is not a source of the same project
                                 --  as the source just compiled. Check if
                                 --  it can be imported.

                                 if not Indirect_Imports then
                                    if Directly_Imports
                                      (Src_Data.Id.Project, Source_2.Project)
                                    then
                                       --  It is a source of a directly
                                       --  imported project. Record its
                                       --  project, for later processing.

                                       Imports.Set
                                         (Source_2.Project, True);

                                    else
                                       --  It is a source of a project that
                                       --  is not directly imported. Record
                                       --  the source for later processing.

                                       Included_Sources.Append (Source_2);
                                    end if;
                                 end if;

                                 if not Source_2.In_Interfaces then
                                    --  It is not a source in the interfaces
                                    --  of its project. Report an error and
                                    --  invalidate the compilation.

                                    Put ('"');
                                    Put
                                      (Get_Name_String
                                         (Src_Data.Id.Path.Display_Name));
                                    Put (""" cannot import """);
                                    Put (Src_Name);
                                    Put_Line (""":");

                                    Put
                                      ("  it is not part of the "
                                       & "interfaces of its project """);
                                    Put
                                      (Get_Name_String
                                         (Source_2.Project.Display_Name));
                                    Put_Line ("""");

                                    Compilation_OK := False;
                                 end if;
                              end if;
                           end if;

                           if First_Src = null then
                              First_Src :=
                                new Src_Record'
                                  (File => new String'(Src_Name),
                                   TS   => Src_TS,
                                   Next => null);
                              Last_Src := First_Src;

                           else
                              Last_Src.Next :=
                                new Src_Record'
                                  (File => new String'(Src_Name),
                                   TS   => Src_TS,
                                   Next => null);
                              Last_Src := Last_Src.Next;
                           end if;
                        end;

                        exit Line_Loop when Finish = Last;

                        --  Go get the next source on the line

                        Start := Finish + 1;

                     end loop Name_Loop;
                  end;

                  --  If we are here, we had a continuation character
                  --  \ at the end of the line, so we continue with
                  --  the next line.

                  Get_Line (Dep_File, Name_Buffer, Name_Len);
                  Start  := 1;
                  Finish := 1;
               end loop Line_Loop;
            end loop Big_Loop;

            Close (Dep_File);

            if Included_Sources.Last > 0 then
               --  Sources in project that are not directly imported
               --  have been found. Check if they may be imported by
               --  other allowed imported sources.

               declare
                  L : Project_List := Src_Data.Id.Project.Imported_Projects;

               begin
                  --  Put in hash table Imports the project trees
                  --  rooted at the projects that are already in
                  --  Imports.

                  while L /= null loop
                     if Imports.Get (L.Project) then
                        Recursive_Import (L.Project);
                     end if;

                     L := L.Next;
                  end loop;

                  --  For all the imported sources from project not
                  --  directly imported, check if their projects are
                  --  in table imports.

                  for J in 1 .. Included_Sources.Last loop
                     declare
                        Included : constant Source_Id :=
                          Included_Sources.Table (J);
                     begin
                        if not Imports.Get (Included.Project) then
                           --  This source is either directly imported or
                           --  imported from another source that should not be
                           --  imported. Report an error and invalidate the
                           --  compilation.

                           Put ('"');
                           Put
                             (Get_Name_String (Src_Data.Id.Path.Display_Name));
                           Put (""" cannot import """);
                           Put
                             (Get_Name_String (Included.Path.Display_Name));
                           Put_Line (""":");

                           Put ("  """);
                           Put
                             (Get_Name_String
                                (Src_Data.Id.Project.Display_Name));
                           Put
                             (""" does not directly import project """);
                           Put
                             (Get_Name_String (Included.Project.Display_Name));
                           Put_Line ("""");

                           Compilation_OK := False;
                        end if;
                     end;
                  end loop;
               end;
            end if;
         end if;

         if Compilation_OK and Dep_File_OK then
            Create (Dep_File, Get_Name_String (Src_Data.Id.Dep_Path));
            Put (Dep_File, Object_Path.all);
            Put (Dep_File, ": ");

            declare
               S : Src_Access := First_Src;
            begin
               while S /= null loop
                  Put (Dep_File, S.File.all);
                  Put (Dep_File, " ");
                  Put (Dep_File, String (S.TS));

                  if S.Next /= null then
                     Put (Dep_File, " \");
                  end if;

                  Put_Line (Dep_File, "");

                  S := S.Next;
               end loop;

               Close (Dep_File);
            end;
         end if;

         Purge;

         return Compilation_OK;
      end Phase_2_Makefile;

      -----------------
      -- Phase_2_ALI --
      -----------------

      function Phase_2_ALI (Src_Data : Queue.Source_Info) return Boolean is
         Compilation_OK : Boolean := True;
         Text           : Text_Buffer_Ptr :=
                            Read_Library_Info_From_Full
                              (File_Name_Type (Src_Data.Id.Dep_Path),
                               Src_Data.Id.Dep_TS'Access);
         The_ALI        : ALI.ALI_Id := ALI.No_ALI_Id;
         Sfile          : File_Name_Type;
         Afile          : File_Name_Type;
         Source_2       : Source_Id;

         procedure Check_Source (Sfile : File_Name_Type);
         --  Check if source Sfile is in the same project file as the Src_Data
         --  source file. Invalidate the compilation if it is not.

         ------------------
         -- Check_Source --
         ------------------

         procedure Check_Source (Sfile : File_Name_Type) is
            Source_3 : constant Source_Id :=
                         Find_Source
                           (Src_Data.Tree, No_Project, Base_Name => Sfile);

         begin
            if Source_3 = No_Source then
               Put ("source ");
               Put (Get_Name_String (Sfile));
               Put_Line (" is not a source of a project");
               Compilation_OK := False;

            elsif Ultimate_Extending_Project_Of (Source_3.Project) /=
              Ultimate_Extending_Project_Of (Src_Data.Id.Project)
            then
               Put ("sources ");
               Put (Get_Name_String (Source_3.File));
               Put (" and ");
               Put (Get_Name_String (Src_Data.Id.File));
               Put (" belong to different projects: ");
               Put (Get_Name_String (Source_3.Project.Display_Name));
               Put (" and ");
               Put_Line (Get_Name_String (Src_Data.Id.Project.Display_Name));
               Compilation_OK := False;
            end if;
         end Check_Source;

      begin
         if Text /= null then
            --  Read the ALI file but read only the necessary lines

            The_ALI :=
              ALI.Scan_ALI
                (File_Name_Type (Src_Data.Id.Dep_Path),
                 Text,
                 Ignore_ED     => False,
                 Err           => True,
                 Read_Lines    => "DW");

            if The_ALI /= ALI.No_ALI_Id then
               for J in ALI.ALIs.Table (The_ALI).First_Unit ..
                 ALI.ALIs.Table (The_ALI).Last_Unit
               loop
                  for K in ALI.Units.Table (J).First_With ..
                    ALI.Units.Table (J).Last_With
                  loop
                     if not
                       ALI.Withs.Table (K).Implicit_With_From_Instantiation
                     then
                        Sfile := ALI.Withs.Table (K).Sfile;

                        --  Skip generics

                        if Sfile /= No_File then

                           --  Look for this source

                           Afile := ALI.Withs.Table (K).Afile;
                           Source_2 := Source_Files_Htable.Get
                             (Src_Data.Tree.Source_Files_HT, Sfile);

                           while Source_2 /= No_Source loop
                              if Is_Compilable (Source_2)
                                and then  Source_2.Dep_Name = Afile
                              then
                                 case Source_2.Kind is
                                 when Spec => null;

                                 when Impl =>
                                    if Is_Subunit (Source_2) then
                                       Source_2 := No_Source;
                                    end if;

                                 when Sep =>
                                    Source_2 := No_Source;
                                 end case;

                                 exit;
                              end if;

                              Source_2 := Source_2.Next_With_File_Name;
                           end loop;

                           --  If it is the source of a project that is not the
                           --  project of the source just compiled, check if it
                           --  is allowed to be imported.

                           if Source_2 /= No_Source then
                              if not Project_Extends
                                (Src_Data.Id.Project, Source_2.Project)
                                and then
                                  not Project_Extends
                                    (Source_2.Project, Src_Data.Id.Project)
                              then
                                 if not Indirect_Imports
                                   and then not Directly_Imports
                                     (Src_Data.Id.Project, Source_2.Project)
                                 then
                                    --  It is in a project that is not directly
                                    --  imported. Report an error and
                                    --  invalidate the compilation.

                                    Put ("Unit """);
                                    Put
                                      (Get_Name_String
                                         (Src_Data.Id.Unit.Name));
                                    Put (""" cannot import unit """);
                                    Put
                                      (Get_Name_String (Source_2.Unit.Name));
                                    Put_Line (""":");

                                    Put ("  """);
                                    Put
                                      (Get_Name_String
                                         (Src_Data.Id.Project.Display_Name));
                                    Put
                                      (""" does not directly"
                                       & " import project """);
                                    Put
                                      (Get_Name_String
                                         (Source_2.Project.Display_Name));
                                    Put_Line ("""");

                                    Compilation_OK := False;

                                 elsif not Source_2.In_Interfaces then
                                    --  It is not an interface of its project.
                                    --  Report an error and invalidate the
                                    --  compilation.

                                    Put ("Unit """);
                                    Put
                                      (Get_Name_String
                                         (Src_Data.Id.Unit.Name));
                                    Put (""" cannot import unit """);
                                    Put
                                      (Get_Name_String (Source_2.Unit.Name));
                                    Put_Line (""":");

                                    Put
                                      ("  it is not part of the "
                                       & "interfaces of its project """);
                                    Put
                                      (Get_Name_String
                                         (Source_2.Project.Display_Name));
                                    Put_Line ("""");
                                    Compilation_OK := False;
                                 end if;
                              end if;
                           end if;
                        end if;
                     end if;
                  end loop;
               end loop;

               if Opt.No_Split_Units then

                  --  Initialized the list of subunits with the unit name

                  Subunits.Init;
                  Subunits.Append
                    (new String'(Get_Name_String (Src_Data.Id.Unit.Name)));

                  --  First check that the spec and the body are in the same
                  --  project.

                  for J in ALI.ALIs.Table (The_ALI).First_Unit ..
                    ALI.ALIs.Table (The_ALI).Last_Unit
                  loop
                     Check_Source (ALI.Units.Table (J).Sfile);
                  end loop;

                  --  Next, check the subunits, if any

                  declare
                     Subunit_Found : Boolean;
                     Already_Found : Boolean;
                     Last          : Positive;
                  begin
                     --  Loop until we don't find new subunits

                     loop
                        Subunit_Found := False;

                        for D in ALI.ALIs.Table (The_ALI).First_Sdep
                          .. ALI.ALIs.Table (The_ALI).Last_Sdep
                        loop
                           if ALI.Sdep.Table (D).Subunit_Name /= No_Name then
                              Get_Name_String
                                (ALI.Sdep.Table (D).Subunit_Name);

                              --  First check if we already found this subunit

                              Already_Found := False;
                              for K in 1 .. Subunits.Last loop
                                 if Name_Buffer (1 .. Name_Len) =
                                   Subunits.Table (K).all
                                 then
                                    Already_Found := True;
                                    exit;
                                 end if;
                              end loop;

                              if not Already_Found then
                                 --  Find the name of the parent

                                 Last := Name_Len - 1;
                                 while Last > 1
                                   and then Name_Buffer (Last + 1) /= '.'
                                 loop
                                    Last := Last - 1;
                                 end loop;

                                 for J in 1 .. Subunits.Last loop
                                    if Subunits.Table (J).all =
                                      Name_Buffer (1 .. Last)
                                    then
                                       --  It is a new subunit, add it o the
                                       --  list and check if it is in the right
                                       --  project.

                                       Subunits.Append
                                         (new String'
                                            (Name_Buffer (1 .. Name_Len)));
                                       Subunit_Found := True;
                                       Check_Source (ALI.Sdep.Table (D).Sfile);
                                       exit;
                                    end if;
                                 end loop;
                              end if;
                           end if;
                        end loop;

                        exit when not Subunit_Found;
                     end loop;
                  end;
               end if;

               if Compilation_OK
                 and then
                   (Builder_Data (Src_Data.Tree).Closure_Needed
                    or else Src_Data.Closure)
               then
                  Record_ALI_For (Src_Data, The_ALI);
               end if;
            end if;

            Free (Text);
         end if;
         return Compilation_OK;
      end Phase_2_ALI;

      --------------------------
      -- Set_Options_For_File --
      --------------------------

      procedure Set_Options_For_File (Id : Source_Id) is

         Config                   : Language_Config renames Id.Language.Config;
         Builder_Options_Instance : constant Builder_Comp_Option_Table_Ref :=
                                      Builder_Compiling_Options_HTable.Get
                                        (Id.Language.Name);
         Comp_Opt                 : constant Comp_Option_Table_Ref :=
                                      Compiling_Options_HTable.Get
                                        (Id.Language.Name);

         List    : Name_List_Index;
         Nam_Nod : Name_Node;
         First   : Boolean;
      begin
         Compilation_Options.Last := 0;

         --  1a) The leading required switches

         List := Config.Compiler_Leading_Required_Switches;
         First := True;
         while List /= No_Name_List loop
            Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);

            if Nam_Nod.Name /= Empty_String then
               Add_Option_Internal_Codepeer
                 (Value   => new String'(Get_Name_String (Nam_Nod.Name)),
                  To      => Compilation_Options,
                  Display => First or Opt.Verbose_Mode);
               First := False;
            end if;

            List := Nam_Nod.Next;
         end loop;

         --  1b) The switches in CodePeer mode

         if Opt.CodePeer_Mode then
            --  Replace -x ada with -x adascil

            declare
               Cur : Integer := Compilation_Options.Last - 1;
            begin
               while Cur > 0
                 and then Compilation_Options.Options (Cur).all /= "-x"
               loop
                  Cur := Cur - 1;
               end loop;

               if Cur /= 0 then
                  Compilation_Options.Visible (Cur) := True;
                  Compilation_Options.Options (Cur + 1) :=
                    new String'("adascil");
                  Compilation_Options.Visible (Cur + 1) := True;

               else
                  Add_Option
                    (Value   => "-x",
                     To      => Compilation_Options,
                     Display => True);

                  Add_Option
                    (Value   => "adascil",
                     To      => Compilation_Options,
                     Display => True);
               end if;
            end;

            Add_Option
              (Value   => "-gnatcC",
               To      => Compilation_Options,
               Display => True);
         end if;

         --  2) the compilation switches specified in package Builder
         --  for all compilers, following "-cargs", if any.

         for Index in 1 .. All_Language_Builder_Compiling_Options.Last loop
            Add_Option_Internal_Codepeer
              (Value   => All_Language_Builder_Compiling_Options.Table (Index),
               To      => Compilation_Options,
               Display => True);
         end loop;

         --  3) the compilation switches specified in package Builder
         --  for the compiler of the language, following
         --  -cargs:<language>.

         if Builder_Options_Instance /= null then
            for Index in 1 ..
              Builder_Compiling_Options.Last (Builder_Options_Instance.all)
            loop
               Add_Option_Internal_Codepeer
                 (Value   => Builder_Options_Instance.Table (Index),
                  To      => Compilation_Options,
                  Display => True);
            end loop;
         end if;

         --  4) The PIC option if it exists, for shared and "static-pic"
         --     libraries.

         if Id.Project.Library
           and then Id.Project.Library_Kind /= Static
         then
            List := Config.Compilation_PIC_Option;
            while List /= No_Name_List loop
               Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);
               Add_Option_Internal_Codepeer
                 (Value   => new String'(Get_Name_String (Nam_Nod.Name)),
                  To      => Compilation_Options,
                  Display => True);
               List := Nam_Nod.Next;
            end loop;
         end if;

         --  5) Compiler'Switches(<source file name>), if it is
         --  defined, otherwise Compiler'Switches (<language name>),
         --  if defined.

         Add_Compilation_Switches (Id);

         --  6) the switches specified on the gprbuild command line
         --  for all compilers, following "-cargs", if any.

         for Index in 1 .. All_Language_Compiling_Options.Last loop
            Add_Option_Internal_Codepeer
              (Value   => All_Language_Compiling_Options.Table (Index),
               To      => Compilation_Options,
               Display => True);
         end loop;

         --  7) the switches specified on the gprbuild command line
         --  for the compiler of the language, following
         --  -cargs:<language>.

         if Comp_Opt /= null then
            for Index in 1 .. Compiling_Options.Last (Comp_Opt.all) loop
               Add_Option_Internal_Codepeer
                 (Value   => Comp_Opt.Table (Index),
                  To      => Compilation_Options,
                  Display => True);
            end loop;
         end if;
      end Set_Options_For_File;

      -------------------------
      -- Check_Switches_File --
      -------------------------

      function Check_Switches_File (Id : Source_Id) return Boolean is

         File : Text_IO.File_Type;

         function Assert_Line (Current : String) return Boolean;
         --  Return False if Current is not the next line in the switches file

         -----------------
         -- Assert_Line --
         -----------------

         function Assert_Line (Current : String) return Boolean is
            Line : String (1 .. 1_000);
            Last : Natural;
         begin
            if End_Of_File (File) then
               if Opt.Verbosity_Level > Opt.Low then
                  Put_Line ("    -> switches file has fewer switches");
               end if;

               Close (File);
               return False;
            end if;

            Get_Line (File, Line, Last);

            if Line (1 .. Last) /= Current then
               if Opt.Verbosity_Level > Opt.Low then
                  Put_Line ("    -> switches file '"
                              & Get_Name_String (Id.Switches_Path)
                              & "' has different line");
                  Put_Line ("       " & Line (1 .. Last));
                  Put_Line ("       " & Current);
               end if;

               Close (File);
               return False;
            end if;
            return True;
         end Assert_Line;

         List    : Name_List_Index;
         Nam_Nod : Name_Node;
      begin
         Open (File, In_File, Get_Name_String (Id.Switches_Path));

         if not Assert_Line (String (Id.Object_TS)) then
            return True;
         end if;

         for Index in 1 .. Compilation_Options.Last loop
            if not Assert_Line (Compilation_Options.Options (Index).all) then
               return True;
            end if;
         end loop;

         List := Id.Language.Config.Compiler_Trailing_Required_Switches;

         while List /= No_Name_List loop
            Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);

            if not Assert_Line (Get_Name_String (Nam_Nod.Name)) then
               return True;
            end if;

            List := Nam_Nod.Next;
         end loop;

         if not End_Of_File (File) then
            if Opt.Verbosity_Level > Opt.Low then
               Put_Line ("    -> switches file has more switches");
            end if;

            Close (File);
            return True;
         end if;

         Close (File);
         return False;

      exception
         when others =>
            if Opt.Verbosity_Level > Opt.Low then
               Put_Line ("    -> no switches file");
            end if;
            return True;
      end Check_Switches_File;

      ------------------------
      -- Update_Object_Path --
      ------------------------

      procedure Update_Object_Path
        (Id : Source_Id; Source_Project : Project_Id) is
      begin
         Id.Object_Project := Source_Project;

         if Id.Object_Project /= Id.Project then
            if Id.Object /= No_File then
               Get_Name_String
                 (Id.Object_Project.Object_Directory.Display_Name);
               Add_Str_To_Name_Buffer (Get_Name_String (Id.Object));
               Id.Object_Path := Name_Find;
            end if;

            if Id.Dep_Name /= No_File then
               Get_Name_String
                 (Id.Object_Project.Object_Directory.Display_Name);
               Add_Str_To_Name_Buffer (Get_Name_String (Id.Dep_Name));
               Id.Dep_Path := Name_Find;
            end if;

            if Id.Switches /= No_File then
               Get_Name_String
                 (Id.Object_Project.Object_Directory.Display_Name);
               Add_Str_To_Name_Buffer (Get_Name_String (Id.Switches));
               Id.Switches_Path := Name_Find;
            end if;
         end if;
      end Update_Object_Path;

      ----------------------------
      -- Add_Dependency_Options --
      ----------------------------

      procedure Add_Dependency_Options (Id : Source_Id) is
         List : Name_List_Index := Id.Language.Config.Dependency_Option;
         Node : Name_Node;
      begin
         if Id.Language.Config.Dependency_Kind /= None then
            while List /= No_Name_List loop
               Node := Project_Tree.Shared.Name_Lists.Table (List);
               List := Node.Next;

               if List = No_Name_List then
                  Add_Option
                    (Value   => Get_Name_String (Node.Name)
                     & Get_Name_String (Id.Dep_Name),
                     To      => Compilation_Options,
                     Display => Opt.Verbose_Mode);
               else
                  Add_Option
                    (Value   => Node.Name,
                     To      => Compilation_Options,
                     Display => Opt.Verbose_Mode);
               end if;
            end loop;
         end if;
      end Add_Dependency_Options;

      ------------------------------
      -- Add_Object_File_Switches --
      ------------------------------

      procedure Add_Object_File_Switches (Id : Source_Id) is
         List : Name_List_Index := Id.Language.Config.Object_File_Switches;
         Node : Name_Node;
      begin
         if List /= No_Name_List then
            loop
               Node := Project_Tree.Shared.Name_Lists.Table (List);
               exit when Node.Next = No_Name_List;

               Add_Option
                 (Node.Name,
                  To      => Compilation_Options,
                  Display => Opt.Verbose_Mode or else Id.Index /= 0);
               List := Node.Next;
            end loop;

            Get_Name_String (Node.Name);
            Add_Str_To_Name_Buffer (Get_Name_String (Id.Object));

            Add_Option
              (Name_Buffer (1 .. Name_Len),
               To      => Compilation_Options,
               Display => Opt.Verbose_Mode or else Id.Index /= 0);

         --  Always specify object-file for a multi-unit source file

         elsif Id.Index /= 0 then
            Add_Option
              ("-o",
               To      => Compilation_Options,
               Display => True);
            Add_Option
              (Get_Name_String (Id.Object),
               To      => Compilation_Options,
               Display => True);
         end if;
      end Add_Object_File_Switches;

      ------------------------------
      -- Add_Object_Path_Switches --
      ------------------------------

      procedure Add_Object_Path_Switches (Id : Source_Id) is
         List : Name_List_Index := Id.Language.Config.Object_Path_Switches;
         Node : Name_Node;

      begin
         if List /= No_Name_List then
            if Id.Project.Object_Path_File = No_Path then
               Create_Object_Path_File (Id.Project, Project_Tree.Shared);
            end if;

            while List /= No_Name_List loop
               Node := Project_Tree.Shared.Name_Lists.Table (List);
               exit when Node.Next = No_Name_List;

               Add_Option
                 (Node.Name,
                  To      => Compilation_Options,
                  Display => Opt.Verbose_Mode);
               List := Node.Next;
            end loop;

            Get_Name_String (Node.Name);
            Add_Str_To_Name_Buffer
              (Get_Name_String (Id.Project.Object_Path_File));

            Add_Option
              (Name_Buffer (1 .. Name_Len),
               To      => Compilation_Options,
               Display => Opt.Verbose_Mode);
         end if;
      end Add_Object_Path_Switches;

      ------------------------------
      -- Add_Config_File_Switches --
      ------------------------------

      procedure Add_Config_File_Switches
        (Id             : Source_Id;
         Source_Project : Project_Id)
      is
         Config           : constant Language_Config := Id.Language.Config;
         Config_File_Path : Path_Name_Type;
      begin
         if Config.Config_File_Switches /= No_Name_List
           and then (Config.Config_Body /= No_Name
                     or else Config.Config_Body_Index /= No_Name
                     or else Config.Config_Body_Pattern /= No_Name
                     or else Config.Config_Spec /= No_Name
                     or else Config.Config_Spec_Index /= No_Name
                     or else Config.Config_Spec_Pattern /= No_Name)
         then
            Create_Config_File
              (For_Project => Source_Project,
               Config      => Config,
               Language    => Id.Language.Name);

            if Source_Project.Config_File_Name /= No_Path then
               Add_Config_File_Switch
                 (Config    => Config,
                  Path_Name => Source_Project.Config_File_Name);
            end if;

            if not Config.Config_File_Unique then
               Config_File_Path :=
                 Config_File_For
                   (Project        => Main_Project,
                    Package_Name   => Name_Builder,
                    Attribute_Name => Name_Global_Config_File,
                    Language       => Id.Language.Name);

               if Config_File_Path /= No_Path then
                  Add_Config_File_Switch
                    (Config    => Config,
                     Path_Name => Config_File_Path);
               end if;

               Config_File_Path :=
                 Config_File_For
                   (Project        => Source_Project,
                    Package_Name   => Name_Compiler,
                    Attribute_Name => Name_Local_Config_File,
                    Language       => Id.Language.Name);

               if Config_File_Path /= No_Path then
                  Add_Config_File_Switch
                    (Config    => Config,
                     Path_Name => Config_File_Path);
               end if;
            end if;
         end if;
      end Add_Config_File_Switches;

      -------------------------------
      -- Add_Mapping_File_Switches --
      -------------------------------

      function Add_Mapping_File_Switches
        (Source         : Queue.Source_Info;
         Source_Project : Project_Id) return Path_Name_Type
      is
         List              : Name_List_Index :=
                               Source.Id.Language.Config.Mapping_File_Switches;
         Node              : Name_Node;
         Mapping_File_Path : Path_Name_Type;
      begin
         if List /= No_Name_List then

            --  Check if there is a temporary mapping file we can use

            Mapping_File_Path := Mapping_Files_Htable.Get_First
              (Source.Id.Language.Mapping_Files);

            if Mapping_File_Path /= No_Path then
               --  Reuse this temporary mapping file and remove its
               --  name from the HTable so that it is not reused
               --  before the compilation terminates.

               Mapping_Files_Htable.Remove
                 (Source.Id.Language.Mapping_Files, Mapping_File_Path);

            else
               --  Create a new temporary mapping file, as there are
               --  none that can be reused.

               GPR.Env.Create_Mapping_File
                 (Project  => Source_Project,
                  Language => Source.Id.Language.Name,
                  In_Tree  => Source.Tree,
                  Name     => Mapping_File_Path);
            end if;

            while List /= No_Name_List loop
               Node := Source.Tree.Shared.Name_Lists.Table (List);
               List := Node.Next;

               if List /= No_Name_List then
                  Add_Option
                    (Value   => Node.Name,
                     To      => Compilation_Options,
                     Display => Opt.Verbose_Mode);

               else
                  Get_Name_String (Node.Name);
                  Add_Str_To_Name_Buffer (Get_Name_String (Mapping_File_Path));
                  Add_Option
                    (Name_Buffer (1 .. Name_Len),
                     To      => Compilation_Options,
                     Display => Opt.Verbose_Mode);
               end if;
            end loop;

            return Mapping_File_Path;

         else
            return No_Path;
         end if;
      end Add_Mapping_File_Switches;

      -----------------------------
      -- Add_Multi_Unit_Switches --
      -----------------------------

      procedure Add_Multi_Unit_Switches (Id : Source_Id) is
         List : Name_List_Index := Id.Language.Config.Multi_Unit_Switches;
      begin
         if Id.Index /= 0
           and then List /= No_Name_List
         then
            declare
               Index_Img : constant String := Id.Index'Img;
               Node      : Name_Node;

            begin
               loop
                  Node := Project_Tree.Shared.Name_Lists.Table (List);
                  exit when Node.Next = No_Name_List;

                  Add_Option
                    (Node.Name,
                     To      => Compilation_Options,
                     Display => True);
                  List := Node.Next;
               end loop;

               Get_Name_String (Node.Name);
               Add_Str_To_Name_Buffer (Index_Img (2 .. Index_Img'Last));
               Add_Option
                 (Name_Buffer (1 .. Name_Len),
                  To      => Compilation_Options,
                  Display => True);
            end;
         end if;
      end Add_Multi_Unit_Switches;

      ---------------------------
      -- Add_Trailing_Switches --
      ---------------------------

      procedure Add_Trailing_Switches (Id : Source_Id) is
         List : Name_List_Index :=
                  Id.Language.Config.Compiler_Trailing_Required_Switches;
         Node : Name_Node;
      begin
         while List /= No_Name_List loop
            Node := Project_Tree.Shared.Name_Lists.Table (List);
            Add_Option
              (Node.Name,
               To      => Compilation_Options,
               Display => Opt.Verbose_Mode);
            List := Node.Next;
         end loop;
      end Add_Trailing_Switches;

      ---------------------------------
      -- Add_Name_Of_Source_Switches --
      ---------------------------------

      procedure Add_Name_Of_Source_Switches (Id : Source_Id) is
         List        : Name_List_Index :=
                         Id.Language.Config.Source_File_Switches;
         Node        : Name_Node;
         Source_Path : OS_Lib.String_Access;
      begin
         --  Add any source file prefix

         if List /= No_Name_List then
            loop
               Node := Project_Tree.Shared.Name_Lists.Table (List);
               exit when Node.Next = No_Name_List;

               Add_Option
                 (Node.Name,
                  To      => Compilation_Options,
                  Display => Opt.Verbose_Mode or else Id.Index /= 0);
               List := Node.Next;
            end loop;
         end if;

         --  Then handle the source file

         Get_Name_String (Id.Path.Display_Name);

--           case Id.Language.Config.Path_Syntax is
--              when Canonical =>
         Source_Path := new String'(Name_Buffer (1 .. Name_Len));

--              when Host =>
--             Source_Path := To_Host_File_Spec (Name_Buffer (1 .. Name_Len));
--           end case;

         if Node.Name = No_Name then
            Add_Option_Internal
              (Source_Path,
               To          => Compilation_Options,
               Display     => True,
               Simple_Name => not Opt.Verbose_Mode);

         else
            Get_Name_String (Node.Name);
            Add_Option
              (Name_Buffer (1 .. Name_Len) & Source_Path.all,
               To          => Compilation_Options,
               Display     => True,
               Simple_Name => not Opt.Verbose_Mode);
         end if;
      end Add_Name_Of_Source_Switches;

      ---------------------------------
      -- Spawn_Compiler_And_Register --
      ---------------------------------

      procedure Spawn_Compiler_And_Register
        (Source                 : Queue.Source_Info;
         Source_Project         : Project_Id;
         Compiler_Path          : String;
         Mapping_File_Path      : Path_Name_Type;
         Last_Switches_For_File : Integer)
      is

         procedure Add_Process
           (Process        : Gprbuild.Compilation.Id;
            Source         : Queue.Source_Info;
            Source_Project : Project_Id;
            Mapping_File   : Path_Name_Type;
            Purpose        : Process_Purpose;
            Options        : String_List_Access);
         --  Add compilation process and indicate that the object directory is
         --  busy.

         procedure Escape_Path (Options : in out String_List);
         --  On Windows, duplicate the directory separators ('\') in Options.
         --  On other platforms, this procedure does nothing.

         -----------------
         -- Add_Process --
         -----------------

         procedure Add_Process
           (Process        : Gprbuild.Compilation.Id;
            Source         : Queue.Source_Info;
            Source_Project : Project_Id;
            Mapping_File   : Path_Name_Type;
            Purpose        : Process_Purpose;
            Options        : String_List_Access)
         is
         begin
            Compilation_Htable.Set
              (Process,
               (Process, Source, Source_Project,
                Mapping_File, Purpose, Options));
            Outstanding_Compiles := Outstanding_Compiles + 1;

            Queue.Set_Obj_Dir_Busy (Source.Id.Project.Object_Directory.Name);
         end Add_Process;

         -----------------
         -- Escape_Path --
         -----------------

         procedure Escape_Path (Options : in out String_List) is
         begin
            if On_Windows then
               for J in Options'Range loop
                  declare
                     Opt : constant String := Options (J).all;
                     Nopt : String (1 .. Opt'Length * 2);
                     Last : Natural := 0;

                  begin
                     for K in Opt'Range loop
                        Last := Last + 1;
                        Nopt (Last) := Opt (K);

                        if Opt (K) = Directory_Separator then
                           Last := Last + 1;
                           Nopt (Last) := Directory_Separator;
                        end if;
                     end loop;

                     if Last > Opt'Length then
                        --  Do not free the option, as it has been recorded in
                        --  All_Options.
                        Options (J) := new String'(Nopt (1 .. Last));
                     end if;
                  end;
               end loop;
            end if;
         end Escape_Path;

         ------------------
         -- Get_Language --
         ------------------

         function Get_Language return String is
           (if Source.Id.Language /= null
            then Get_Name_String (Source.Id.Language.Name)
            else "");

         Process       : Gprbuild.Compilation.Id;
         Options       : GNAT.OS_Lib.Argument_List_Access;
         Response_File : Path_Name_Type := No_Path;

      --  Start of processing of Spawn_Compiler_And_Register

      begin
         if not Opt.Quiet_Output then
            Name_Len := 0;

            if Opt.Verbose_Mode then
               Add_Str_To_Name_Buffer (Compiler_Path);

               for Option in 1 .. Compilation_Options.Last loop
                  Add_Str_To_Name_Buffer (" ");

                  if Compilation_Options.Simple_Name (Option) then
                     Add_Str_To_Name_Buffer
                       (Base_Name (Compilation_Options.Options (Option).all));

                  else
                     Add_Str_To_Name_Buffer
                       (Compilation_Options.Options (Option).all);
                  end if;
               end loop;

               Put_Line (Name_Buffer (1 .. Name_Len));

            else
               Display
                 (Section  => GPR.Compile,
                  Command  =>
                    Get_Name_String (Source.Id.Language.Display_Name),
                  Argument => Get_Name_String (Source.Id.File));
            end if;
         end if;

         if Source_Project.Config.Max_Command_Line_Length > 0 and then
           Source.Id.Language.Config.Resp_File_Format = GCC_GNU
         then
            declare
               Arg_Length : Natural := 0;
            begin
               for J in 1 .. Compilation_Options.Last loop
                  Arg_Length :=
                    Arg_Length + 1 + Compilation_Options.Options (J)'Length;
               end loop;

               if Arg_Length > Source_Project.Config.Max_Command_Line_Length
               then
                  declare
                     use GPR.Tempdir;
                     FD : File_Descriptor;
                     Status    : Integer;
                     Closing_Status : Boolean;

                  begin
                     --  On Windows, directory separators ('\') need to be
                     --  doubled in response files, otherwise gcc does not
                     --  take them as directory separators.

                     Escape_Path
                       (Compilation_Options.Options
                          (1 .. Compilation_Options.Last));

                     Create_Temp_File (FD, Response_File);
                     Record_Temp_File
                       (Shared => Source.Tree.Shared,
                        Path   => Response_File);

                     Option_Loop :
                     for J in 1 .. Compilation_Options.Last loop
                        Status :=
                          Write
                            (FD,
                             Compilation_Options.Options (J) (1)'Address,
                             Compilation_Options.Options (J)'Length);

                        if Status /= Compilation_Options.Options (J)'Length
                        then
                           Put_Line
                             ("Could not write option """ &
                              Compilation_Options.Options (J).all &
                              """ in response file """ &
                              Get_Name_String (Response_File) &
                              """");
                           Response_File := No_Path;
                           exit Option_Loop;
                        end if;

                        Status := Write (FD, ASCII.LF'Address, 1);
                     end loop Option_Loop;

                     Close (FD, Closing_Status);

                     if not Closing_Status and then Response_File /= No_Path
                     then
                        Put_Line
                          ("Could not close response file """ &
                           Get_Name_String (Response_File) &
                           """");
                        Response_File := No_Path;
                     end if;
                  end;

                  if Opt.Verbosity_Level > Opt.Low and then
                    Response_File /= No_Path
                  then
                     Put_Line ("using a response file");
                  end if;
               end if;
            end;
         end if;

         Process := Run
           (Compiler_Path,
            Compilation_Options.Options (1 .. Compilation_Options.Last),
            Source_Project,
            Source => Get_Name_String (Source.Id.File),
            Language => Get_Language,
            Dep_Name => (if Source.Id.Dep_Name = No_File
                         then ""
                         else Get_Name_String (Source.Id.Dep_Name)),
            Obj_Name => (if Source.Id.Object = No_File
                         then ""
                         else Get_Name_String (Source.Id.Object)),
            Response_File => Response_File);

         if Last_Switches_For_File >= 0 then
            Compilation_Options.Last := Last_Switches_For_File;
            Add_Trailing_Switches (Source.Id);
            Options :=
              new String_List'
                (Compilation_Options.Options (1 .. Compilation_Options.Last));
         end if;

         Add_Process
           (Process        => Process,
            Source         => Source,
            Source_Project => Source_Project,
            Mapping_File   => Mapping_File_Path,
            Purpose        => Compilation,
            Options        => Options);
      end Spawn_Compiler_And_Register;

      ------------------------------
      -- Get_Compatible_Languages --
      ------------------------------

      function Get_Compatible_Languages
        (Lang : Language_Ptr) return Name_Ids
      is
         NL        : Name_List_Index :=
                       Lang.Config.Include_Compatible_Languages;
         Languages : Name_Ids
           (1 .. 1 + Length (Project_Tree.Shared.Name_Lists, NL));
         Index     : Positive := 1;
      begin
         Languages (Index) := Lang.Name;

         while NL /= No_Name_List loop
            Index := Index + 1;
            Languages (Index) :=
              Project_Tree.Shared.Name_Lists.Table (NL).Name;
            NL := Project_Tree.Shared.Name_Lists.Table (NL).Next;
         end loop;

         return Languages;
      end Get_Compatible_Languages;

      -------------------------------
      -- Prepare_Include_Path_File --
      -------------------------------

      procedure Prepare_Include_Path_File
        (Data    : out Local_Project_Data;
         Project : Project_Id;
         Lang    : Language_Ptr)
      is
         FD     : File_Descriptor;
         Status : Boolean;
      begin
         Get_Directories
           (Project_Tree => Project_Tree,
            For_Project  => Project,
            Activity     => Compilation,
            Languages    => Get_Compatible_Languages (Lang));

         GPR.Env.Create_New_Path_File
           (Shared    => Project_Tree.Shared,
            Path_FD   => FD,
            Path_Name => Data.Include_Path_File);

         if FD = Invalid_FD then
            Fail_Program
              (Project_Tree, "could not create temporary path file");
         end if;

         for Index in 1 .. Directories.Last loop
            Get_Name_String (Directories.Table (Index));
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := ASCII.LF;
            if Write (FD, Name_Buffer (1)'Address, Name_Len) /= Name_Len then
               Fail_Program
                 (Project_Tree,
                  "disk full when writing include path file");
            end if;
         end loop;

         Close (FD, Status);

         if not Status then
            Fail_Program
              (Project_Tree,
               "disk full when writing include path file");
         end if;
      end Prepare_Include_Path_File;

      ------------------------------------
      -- Prepare_Imported_Dirs_Switches --
      ------------------------------------

      procedure Prepare_Imported_Dirs_Switches
        (Data    : out Local_Project_Data;
         Project : Project_Id;
         Lang    : Language_Ptr)
      is
         Len       : constant Natural :=
                       Length
                         (Project_Tree.Shared.Name_Lists,
                          Lang.Config.Include_Option);
--           Host_Path : OS_Lib.String_Access;
         Last      : Natural := 0;
         List      : Name_List_Index;
         Nam       : Name_Node;
      begin
         Get_Directories
           (Project_Tree => Project_Tree,
            For_Project  => Project,
            Activity     => Compilation,
            Languages    => Get_Compatible_Languages (Lang));

         Free (Data.Imported_Dirs_Switches);
         Data.Imported_Dirs_Switches :=
           new String_List (1 .. Directories.Last * Len);

         for Index in 1 .. Directories.Last loop
            List := Lang.Config.Include_Option;
            while List /= No_Name_List loop
               Nam := Project_Tree.Shared.Name_Lists.Table (List);
               exit when Nam.Next = No_Name_List;
               Last := Last + 1;
               Data.Imported_Dirs_Switches (Last) :=
                 new String'(Get_Name_String (Nam.Name));
               List := Nam.Next;
            end loop;

            Get_Name_String (Directories.Table (Index));

            while Name_Len > 1
              and then (Name_Buffer (Name_Len) = Directory_Separator
                        or else Name_Buffer (Name_Len) = '/')
            loop
               Name_Len := Name_Len - 1;
            end loop;

            Last := Last + 1;

            --  Concatenate the last switch and the path in a single option

--              case Lang.Config.Path_Syntax is
--                 when Canonical =>
            Data.Imported_Dirs_Switches (Last) := new String'
              (Get_Name_String (Nam.Name) & Name_Buffer (1 .. Name_Len));

--                 when Host =>
--                    Host_Path := To_Host_Dir_Spec
--                      (Name_Buffer (1 .. Name_Len), False);
--                    Data.Imported_Dirs_Switches (Last) := new String'
--                      (Get_Name_String (Nam.Name) & Host_Path.all);
--                    Free (Host_Path);
--              end case;
         end loop;
      end Prepare_Imported_Dirs_Switches;

      ------------------------------
      -- Set_Env_For_Include_Dirs --
      ------------------------------

      procedure Set_Env_For_Include_Dirs
        (Id : Source_Id; Source_Project : Project_Id)
      is
         Data : Local_Project_Data :=
                  Local_Projects_HT.Get (Local_Projects, Id.Object_Project);
      begin
         --  Prepare (if not already done) the data for Project/Lang.
         --  All files for a given language are processed sequentially, before
         --  we switch to the next language, so we are only preparing once per
         --  language here.

         if Data.Include_Language /= Id.Language then
            Free (Data.Include_Path);
            Free (Data.Imported_Dirs_Switches);
            Data := No_Local_Project_Data;

            if Id.Language.Config.Include_Option /= No_Name_List then
               Prepare_Imported_Dirs_Switches
                 (Data, Id.Object_Project, Id.Language);

            elsif Id.Language.Config.Include_Path_File /= No_Name then
               if Id.Language.Config.Mapping_File_Switches = No_Name_List
                 or else Opt.Use_Include_Path_File
               then
                  Prepare_Include_Path_File
                    (Data, Id.Object_Project, Id.Language);
               end if;

            elsif Id.Language.Config.Include_Path /= No_Name then
               Get_Directories
                 (Project_Tree => Project_Tree,
                  For_Project  => Id.Object_Project,
                  Activity     => Compilation,
                  Languages    => Get_Compatible_Languages (Id.Language));
               Data.Include_Path := Create_Path_From_Dirs;
            end if;

            Data.Include_Language := Id.Language;

            Local_Projects_HT.Set (Local_Projects, Id.Object_Project, Data);
         end if;

         --  Reset environment variables if they have changed

         if Id.Object_Project /= Current_Project
           or else Id.Language /= Current_Language_Ind
         then
            Current_Project      := Id.Object_Project;
            Current_Language_Ind := Id.Language;

            if Data.Include_Path_File /= No_Path then
               Setenv (Get_Name_String (Id.Language.Config.Include_Path_File),
                       Get_Name_String (Data.Include_Path_File));

            elsif Data.Include_Path /= null then
               Gprbuild.Compilation.Process.Record_Environment
                 (Source_Project,
                  Id.Language.Name,
                  Get_Name_String (Id.Language.Config.Include_Path),
                  Data.Include_Path.all);

               if Opt.Verbosity_Level > Opt.Low then
                  Put
                    (Get_Name_String (Id.Language.Config.Include_Path));
                  Put (" = ");
                  Put_Line (Data.Include_Path.all);
               end if;
            end if;
         end if;

         --  But always set the switches

         if Data.Imported_Dirs_Switches /= null then
            for J in Data.Imported_Dirs_Switches'Range loop
               if Data.Imported_Dirs_Switches (J)'Length > 0 then
                  Add_Option_Internal
                    (Value   => Data.Imported_Dirs_Switches (J),
                     To      => Compilation_Options,
                     Display => Opt.Verbose_Mode);
               end if;
            end loop;
         end if;
      end Set_Env_For_Include_Dirs;

      -----------------------------
      -- Process_Project_Phase_1 --
      -----------------------------

      procedure Process_Project_Phase_1 (Source : Queue.Source_Info) is
         Id                     : constant Source_Id := Source.Id;
         Project_Tree           : constant Project_Tree_Ref := Source.Tree;
         Source_Project         : constant Project_Id :=
                                    Ultimate_Extending_Project_Of
                                      (Source.Id.Project);
         Compilation_Needed     : Boolean := True;
         Last_Switches_For_File : Integer;
         Mapping_File           : Path_Name_Type;
         The_ALI                : ALI.ALI_Id;

      begin
         if Always_Compile or else not Source_Project.Externally_Built then
            Need_To_Compile
              (Source         => Source.Id,
               Tree           => Source.Tree,
               In_Project     => Source_Project,
               Must_Compile   => Compilation_Needed,
               The_ALI        => The_ALI,
               Object_Check   => Object_Checked,
               Always_Compile => Always_Compile);

            if Compilation_Needed and then Opt.Keep_Going then
               --  When in Keep_Going mode first check that we did not already
               --  tried to compile this source as part of another import of
               --  the corresponding project file.

               if Bad_Compilations.Contains (Source.Id) then
                  Compilation_Needed := False;
               end if;
            end if;

            if Compilation_Needed or else Opt.Check_Switches then
               Set_Options_For_File (Source.Id);

               if Opt.Check_Switches and then not Compilation_Needed then
                  Compilation_Needed := Check_Switches_File (Source.Id);
               end if;
            end if;

            if Compilation_Needed then
               --  If Distributed_Mode activated, parse Remote package to
               --  register and initialize the slaves.

               if Distributed_Mode and then not Slave_Initialized then
                  begin
                     Gprbuild.Compilation.Slave.Register_Remote_Slaves
                       (Project_Tree, Main_Project);
                     Slave_Initialized := True;
                  exception
                     when E : Constraint_Error =>
                        Fail_Program (Project_Tree, Exception_Message (E));
                  end;
               end if;

               Update_Object_Path (Source.Id, Source_Project);
               Change_To_Object_Directory (Source_Project);

               --  Record the last recorded option index, to be able to
               --  write the switches file later.

               if Id.Language.Config.Object_Generated then
                  Last_Switches_For_File := Compilation_Options.Last;
               else
                  Last_Switches_For_File := -1;
               end if;

               Add_Dependency_Options (Id);
               Set_Env_For_Include_Dirs (Id, Source_Project);
               Add_Config_File_Switches (Id, Source_Project);
               Mapping_File := Add_Mapping_File_Switches
                 (Source, Source_Project);
               Add_Trailing_Switches (Id);
               Add_Name_Of_Source_Switches (Id);
               Add_Object_File_Switches (Id);
               Add_Multi_Unit_Switches (Id);
               Add_Object_Path_Switches (Id);

               Spawn_Compiler_And_Register
                 (Source                 => Source,
                  Source_Project         => Source_Project,
                  Compiler_Path          =>
                    Get_Compiler_Driver_Path (Project_Tree, Id.Language).all,
                  Mapping_File_Path      => Mapping_File,
                  Last_Switches_For_File => Last_Switches_For_File);

            else
               Print_Compilation_Outputs (Source.Id);

               if Builder_Data (Source.Tree).Closure_Needed
                 and then
                   (Id.Language.Config.Dependency_Kind = ALI_File
                    or else Id.Language.Config.Dependency_Kind = ALI_Closure)
               then
                  Record_ALI_For (Source, The_ALI);

               else
                  ALI.Initialize_ALI;
                  --  ALI.Util.Initialize_ALI_Source;
               end if;
            end if;
         end if;
      end Process_Project_Phase_1;

      --------------------------------
      -- Must_Exit_Because_Of_Error --
      --------------------------------

      function Must_Exit_Because_Of_Error return Boolean is
         use type Containers.Count_Type;

         Source_Identity : Queue.Source_Info;
         Compilation_OK  : Boolean;
         Slave           : Unbounded_String;

         Cur : Bad_Compilations_Set.Cursor;
         OK  : Boolean;
      begin
         if not Bad_Compilations.Is_Empty and then not Opt.Keep_Going then
            while Outstanding_Compiles > 0 loop
               Await_Compile (Source_Identity, Compilation_OK, Slave);

               if not Compilation_OK then
                  Bad_Compilations.Insert
                    (Source_Identity.Id, To_String (Slave), Cur, OK);
               end if;
            end loop;

            return True;
         end if;
         return False;
      end Must_Exit_Because_Of_Error;

      -------------------------------
      -- Start_Compile_If_Possible --
      -------------------------------

      procedure Start_Compile_If_Possible is
         Found  : Boolean;
         Source : Queue.Source_Info;
      begin
         if not Queue.Is_Empty
           and then Outstanding_Compiles < Get_Maximum_Processes
         then
            Queue.Extract (Found, Source);
            if Found then
               Initialize_Source_Record (Source.Id);
               Process_Project_Phase_1 (Source);
            end if;
         end if;
      end Start_Compile_If_Possible;

      -----------------------------
      -- Wait_For_Available_Slot --
      -----------------------------

      procedure Wait_For_Available_Slot is
         Source_Identity : Queue.Source_Info;
         Compilation_OK  : Boolean;
         No_Check        : Boolean;
         Slave           : Unbounded_String;
         use Queue;

         Cur : Bad_Compilations_Set.Cursor;
         OK  : Boolean;
      begin
         if Outstanding_Compiles = Get_Maximum_Processes
           or else (Queue.Is_Virtually_Empty and then Outstanding_Compiles > 0)
         then
            Await_Compile (Source_Identity, Compilation_OK, Slave);

            if Compilation_OK
              and then Source_Identity /= Queue.No_Source_Info
            then
               --  Check if dependencies are on sources in Interfaces and,
               --  when --direct-import-only is used, the imported sources
               --  come from directly withed projects.

               Imports.Reset;
               Included_Sources.Set_Last (0);

               case Source_Identity.Id.Language.Config.Dependency_Kind is
                  when None     => null;
                  when Makefile =>
                     Compilation_OK := Phase_2_Makefile (Source_Identity);
                  when ALI_File | ALI_Closure =>
                     Compilation_OK := Phase_2_ALI (Source_Identity);
               end case;

               --  If the compilation was invalidated, delete the compilation
               --  artifacts.

               if not Compilation_OK then
                  if Source_Identity.Id.Dep_Path /= No_Path then
                     Delete_File
                       (Get_Name_String (Source_Identity.Id.Dep_Path),
                        No_Check);
                  end if;

                  if Source_Identity.Id.Object_Path /= No_Path then
                     Delete_File
                       (Get_Name_String (Source_Identity.Id.Object_Path),
                        No_Check);
                  end if;

                  if Source_Identity.Id.Switches_Path /= No_Path then
                     Delete_File
                       (Get_Name_String (Source_Identity.Id.Switches_Path),
                        No_Check);
                  end if;
               end if;
            end if;

            if not Compilation_OK then
               Bad_Compilations.Insert
                 (Source_Identity.Id, To_String (Slave), Cur, OK);
            end if;
         end if;
      end Wait_For_Available_Slot;

   --  Start of processing for Compilation_Phase

   begin
      Outstanding_Compiles := 0;

      --  Then process each files in the queue (new files might be added to
      --  the queue as a result).

      Compilation_Loop :
      while not Queue.Is_Empty or else Outstanding_Compiles > 0 loop
         exit Compilation_Loop when Must_Exit_Because_Of_Error;
         Start_Compile_If_Possible;
         Wait_For_Available_Slot;

         if Opt.Display_Compilation_Progress then
            Put_Line
              ("completed" &
               Queue.Processed'Img &
               " out of" &
               Queue.Size'Img &
               " (" &
               Trim
                 (Source => Int (((Queue.Processed) * 100) / Queue.Size)'Img,
                  Side => Ada.Strings.Left) &
               "%)...");
         end if;
      end loop Compilation_Loop;

      --  Release local memory

      declare
         Data : Local_Project_Data :=
                  Local_Projects_HT.Get_First (Local_Projects);
      begin
         while Data /= No_Local_Project_Data loop
            Free (Data.Include_Path);
            Free (Data.Imported_Dirs_Switches);
            Data := Local_Projects_HT.Get_Next (Local_Projects);
         end loop;

         Local_Projects_HT.Reset (Local_Projects);
      end;
   end Compilation_Phase;

   ---------------------
   -- Project_Extends --
   ---------------------

   function Project_Extends
     (Extending : Project_Id;
      Extended  : Project_Id) return Boolean
   is
      Current : Project_Id := Extending;
   begin
      loop
         if Current = No_Project then
            return False;

         elsif Current = Extended then
            return True;
         end if;

         Current := Current.Extends;
      end loop;
   end Project_Extends;

end Gprbuild.Compile;
