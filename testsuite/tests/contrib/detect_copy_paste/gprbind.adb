------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2006-2016, AdaCore                     --
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

--  gprbind is the executable called by gprmake to bind Ada sources. It is
--  the driver for gnatbind. It gets its input from gprmake through the
--  binding exchange file and gives back its results through the same file.

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;      use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Gprexch;        use Gprexch;
with Gpr_Build_Util; use Gpr_Build_Util;
with Gpr_Script;     use Gpr_Script;
with Gpr_Util;       use Gpr_Util;
with GPR;            use GPR;
with GPR.ALI;        use GPR.ALI;
with GPR.Names;      use GPR.Names;
with GPR.Osint;      use GPR.Osint;
with GPR.Tempdir;
with GNAT.Table;
with GPR.Util;       use GPR.Util;

procedure Gprbind is

   Shared_Libgcc_Default : Character;
   for Shared_Libgcc_Default'Size use Character'Size;
   pragma Import
     (C, Shared_Libgcc_Default, "__gnat_shared_libgcc_default");

   Executable_Suffix : constant String_Access := Get_Executable_Suffix;
   --  The suffix of executables on this platforms

   GNATBIND : String_Access := new String'("gnatbind");
   --  The file name of the gnatbind executable. May be modified by an option
   --  in the Minimum_Binder_Options.

   Gnatbind_Prefix_Equal : constant String := "gnatbind_prefix=";
   --  Start of the option to specify a prefix for the gnatbind executable

   Gnatbind_Path_Equal : constant String := "--gnatbind_path=";
   --  Start of the option to specify the absolute path of gnatbind

   Ada_Binder_Equal : constant String := "ada_binder=";
   --  Start of the option to specify the full name of the Ada binder
   --  executable. Introduced for GNAAMP, where it is gnaambind.

   Quiet_Output        : Boolean := False;
   Verbose_Low_Mode    : Boolean := False;
   Verbose_Higher_Mode : Boolean := False;

   Dash_O_Specified      : Boolean := False;
   Dash_O_File_Specified : Boolean := False;

   There_Are_Stand_Alone_Libraries : Boolean := False;
   --  Set to True if the corresponding label is in the exchange file

   No_Main_Option : constant String := "-n";
   Dash_o         : constant String := "-o";
   Dash_shared    : constant String := "-shared";
   Dash_x         : constant String := "-x";
   Dash_Fequal    : constant String := "-F=";
   Dash_OO        : constant String := "-O";

   --  Minimum switches to be used to compile the binder generated file

   Dash_c      : constant String := "-c";
   Dash_gnatA  : constant String := "-gnatA";
   Dash_gnatWb : constant String := "-gnatWb";
   Dash_gnatiw : constant String := "-gnatiw";
   Dash_gnatws : constant String := "-gnatws";

   GCC_Version : Character := '0';
   Gcc_Version_String : constant String := "gcc version ";

   Shared_Libgcc : constant String := "-shared-libgcc";
   Static_Libgcc : constant String := "-static-libgcc";

   Libgcc_Specified : Boolean := False;
   --  True if -shared-libgcc or -static-libgcc is used

   IO_File : File_Type;
   --  The file to get the inputs and to put the results of the binding

   Line : String (1 .. 1_000);
   Last : Natural;

   Exchange_File_Name : String_Access;
   Ada_Compiler_Path  : String_Access;
   FULL_GNATBIND      : String_Access;
   Gnatbind_Path      : String_Access;
   Gnatbind_Path_Specified : Boolean := False;

   Compiler_Options     : String_List_Access := new String_List (1 .. 100);
   Last_Compiler_Option : Natural := 0;
   Compiler_Trailing_Options : String_List_Access := new String_List (1 .. 10);
   Last_Compiler_Trailing_Option : Natural := 0;

   Gnatbind_Options     : String_List_Access := new String_List (1 .. 100);
   Last_Gnatbind_Option : Natural := 0;

   Main_ALI : String_Access := null;

   Main_Base_Name        : String_Access := null;
   Binder_Generated_File : String_Access := null;
   BG_File               : File_Type;

   Mapping_File : String_Access := null;

   Success     : Boolean := False;
   Return_Code : Integer;

   Adalib_Dir  : String_Access;
   Prefix_Path : String_Access;
   Lib_Path    : String_Access;

   Static_Libs : Boolean := True;

   Current_Section : Binding_Section := No_Binding_Section;

   All_Binding_Options : Boolean;
   Get_Option          : Boolean;
   Xlinker_Seen        : Boolean;
   Stack_Equal_Seen    : Boolean;

   GNAT_Version : String_Access := new String'("000");
   --  The version of GNAT, coming from the Toolchain_Version for Ada

   GNAT_Version_Set : Boolean := False;
   --  True when the toolchain version is in the input exchange file

   Delete_Temp_Files : Boolean := True;

   FD_Objects   : File_Descriptor;
   Objects_Path : Path_Name_Type;
   Objects_File : File_Type;

   Ada_Object_Suffix : String_Access := Get_Object_Suffix;

   Display_Line : String_Access := new String (1 .. 1_000);
   Display_Last : Natural := 0;
   --  A String buffer to store temporarily the displayed gnatbind command
   --  invoked by gprbind.

   procedure Add_To_Display_Line (S : String);
   --  Add an argument to the Display_Line

   package Binding_Options_Table is new GNAT.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);

   Binding_Option_Dash_V_Specified : Boolean := False;
   --  Set to True if -v is specified in the binding options

   GNAT_6_Or_Higher   : Boolean := False;
   --  Set to True when GNAT version is neither 3.xx nor 5.xx

   GNAT_6_4_Or_Higher : Boolean := False;
   --  Set to True when GNAT_6_Or_Higher is True and if GNAT version is 6.xy
   --  with x >= 4.

   package ALI_Files_Table is new GNAT.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);

   type Path_And_Stamp is record
      Path : String_Access;
      Stamp : String_Access;
   end record;

   package Project_Paths is new GNAT.Table
     (Table_Component_Type => Path_And_Stamp,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);

   type Bound_File;
   type Bound_File_Access is access Bound_File;
   type Bound_File is record
      Name : String_Access;
      Next : Bound_File_Access;
   end record;

   Bound_Files : Bound_File_Access;

   -------------------------
   -- Add_To_Display_Line --
   -------------------------

   procedure Add_To_Display_Line (S : String) is
   begin
      while Display_Last + 1 + S'Length > Display_Line'Last loop
         declare
            New_Buffer : constant String_Access :=
              new String (1 .. 2 * Display_Line'Length);
         begin
            New_Buffer (1 .. Display_Last) :=
              Display_Line (1 .. Display_Last);
            Free (Display_Line);
            Display_Line := New_Buffer;
         end;
      end loop;

      if Display_Last > 0 then
         Display_Last := Display_Last + 1;
         Display_Line (Display_Last) := ' ';
      end if;

      Display_Line (Display_Last + 1 .. Display_Last + S'Length) := S;
      Display_Last := Display_Last + S'Length;
   end Add_To_Display_Line;

begin
   Set_Program_Name ("gprbind");

   --  As the section header has alreading been displayed when gprlib was
   --  invoked, indicate that it should not be displayed again.

   GPR.Set (Section => GPR.Bind);

   if Argument_Count /= 1 then
      Fail_Program (null, "incorrect invocation");
   end if;

   Exchange_File_Name := new String'(Argument (1));

   --  DEBUG: save a copy of the exchange file

   declare
      Gprbind_Debug : constant String := Getenv ("GPRBIND_DEBUG").all;

   begin
      if Gprbind_Debug = "TRUE" then
         Copy_File
           (Exchange_File_Name.all,
            Exchange_File_Name.all & "__saved",
            Success,
            Mode => Overwrite,
            Preserve => Time_Stamps);
      end if;
   end;

   --  Open the binding exchange file

   begin
      Open (IO_File, In_File, Exchange_File_Name.all);
   exception
      when others =>
         Fail_Program (null, "could not read " & Exchange_File_Name.all);
   end;

   --  Get the information from the binding exchange file

   while not End_Of_File (IO_File) loop
      Get_Line (IO_File, Line, Last);

      if Last > 0 then
         if Line (1) = '[' then
            Current_Section := Get_Binding_Section (Line (1 .. Last));

            case Current_Section is
               when No_Binding_Section =>
                  Fail_Program
                    (null, "unknown section: " & Line (1 .. Last));

               when Quiet =>
                  Quiet_Output        := True;
                  Verbose_Low_Mode    := False;
                  Verbose_Higher_Mode := False;

               when Verbose_Low =>
                  Quiet_Output        := False;
                  Verbose_Low_Mode    := True;
                  Verbose_Higher_Mode := False;

               when Verbose_Higher =>
                  Quiet_Output        := False;
                  Verbose_Low_Mode    := True;
                  Verbose_Higher_Mode := True;

               when Shared_Libs =>
                  Static_Libs := False;

               when Gprexch.There_Are_Stand_Alone_Libraries =>
                  There_Are_Stand_Alone_Libraries := True;

               when others =>
                  null;
            end case;

         else
            case Current_Section is
               when No_Binding_Section =>
                  Fail_Program
                    (null, "no section specified: " & Line (1 .. Last));

               when Quiet =>
                  Fail_Program (null, "quiet section should be empty");

               when Verbose_Low | Verbose_Higher =>
                  Fail_Program (null, "verbose section should be empty");

               when Shared_Libs =>
                  Fail_Program
                    (null, "shared libs section should be empty");

               when Gprexch.There_Are_Stand_Alone_Libraries =>
                  Fail_Program
                    (null, "stand-alone libraries section should be empty");

               when Gprexch.Main_Base_Name =>
                  if Main_Base_Name /= null then
                     Fail_Program
                       (null, "main base name specified multiple times");
                  end if;

                  Main_Base_Name := new String'(Line (1 .. Last));

               when Gprexch.Mapping_File =>
                  Mapping_File := new String'(Line (1 .. Last));

               when Compiler_Path =>
                  if Ada_Compiler_Path /= null then
                     Fail_Program
                       (null, "compiler path specified multiple times");
                  end if;

                  Ada_Compiler_Path := new String'(Line (1 .. Last));

               when Compiler_Leading_Switches =>
                  Add
                    (Line (1 .. Last),
                     Compiler_Options, Last_Compiler_Option);

               when Compiler_Trailing_Switches =>
                  Add
                    (Line (1 .. Last),
                     Compiler_Trailing_Options, Last_Compiler_Trailing_Option);

               when Main_Dependency_File =>
                  if Main_ALI /= null then
                     Fail_Program
                       (null, "main ALI file specified multiple times");
                  end if;

                  Main_ALI := new String'(Line (1 .. Last));

               when Dependency_Files =>
                  ALI_Files_Table.Append (new String'(Line (1 .. Last)));

               when Binding_Options =>
                  --  Check if a gnatbind absolute is specified

                  if Last > Gnatbind_Path_Equal'Length
                    and then Line (1 .. Gnatbind_Path_Equal'Length) =
                             Gnatbind_Path_Equal
                  then
                     Gnatbind_Path := new String'
                       (Line (Gnatbind_Path_Equal'Length + 1 .. Last));
                     Gnatbind_Path_Specified := True;

                  --  Check if a gnatbind prefix is specified

                  elsif Last >= Gnatbind_Prefix_Equal'Length
                    and then Line (1 .. Gnatbind_Prefix_Equal'Length) =
                             Gnatbind_Prefix_Equal
                  then
                     --  Ignore an empty prefix

                     if Last > Gnatbind_Prefix_Equal'Length then
                        --  There is always a '-' between <prefix> and
                        --  "gnatbind". Add one if not already in <prefix>.

                        if Line (Last) /= '-' then
                           Last := Last + 1;
                           Line (Last) := '-';
                        end if;

                        GNATBIND := new String'
                          (Line (Gnatbind_Prefix_Equal'Length + 1 .. Last) &
                           "gnatbind");
                     end if;

                  elsif Last > Ada_Binder_Equal'Length
                    and then Line (1 .. Ada_Binder_Equal'Length) =
                             Ada_Binder_Equal
                  then
                     GNATBIND := new String'
                       (Line (Ada_Binder_Equal'Length + 1 .. Last));

                  --  When -O is used, instead of -O=file, -v is ignored to
                  --  avoid polluting the output. Record occurence of -v and
                  --  check the GNAT version later.

                  elsif Line (1 .. Last) = "-v" then
                     Binding_Option_Dash_V_Specified := True;

                  --  Ignore -C, as the generated sources are always in Ada

                  elsif  Line (1 .. Last) /= "-C" then
                     Binding_Options_Table.Append
                                             (new String'(Line (1 .. Last)));
                  end if;

               when Project_Files =>
                  if End_Of_File (IO_File) then
                     Fail_Program
                       (null, "no time stamp for " & Line (1 .. Last));

                  else
                     declare
                        PS : Path_And_Stamp;

                     begin
                        PS.Path := new String'(Line (1 .. Last));
                        Get_Line (IO_File, Line, Last);
                        PS.Stamp := new String'(Line (1 .. Last));
                        Project_Paths.Append (PS);
                     end;
                  end if;

               when Gprexch.Toolchain_Version =>
                  if End_Of_File (IO_File) then
                     Fail_Program
                       (null,
                        "no toolchain version for language " &
                        Line (1 .. Last));

                  elsif Line (1 .. Last) = "ada" then
                     Get_Line (IO_File, Line, Last);

                     if Last > 5 and then Line (1 .. 5) = "GNAT " then
                        GNAT_Version := new String'(Line (6 .. Last));
                        GNAT_Version_Set := True;
                     end if;

                  else
                     Skip_Line (IO_File);
                  end if;

               when Gprexch.Delete_Temp_Files =>
                  begin
                     Delete_Temp_Files := Boolean'Value (Line (1 .. Last));

                  exception
                     when Constraint_Error =>
                        null;
                  end;

               when Gprexch.Object_File_Suffix =>
                  if End_Of_File (IO_File) then
                     Fail_Program
                       (null,
                        "no object file suffix for language " &
                        Line (1 .. Last));

                  elsif Line (1 .. Last) = "ada" then
                     Get_Line (IO_File, Line, Last);
                     Ada_Object_Suffix := new String'(Line (1 .. Last));

                  else
                     Skip_Line (IO_File);
                  end if;

               when Script_Path =>
                  Build_Script_Name := new String'(Line (1 .. Last));

               when Nothing_To_Bind        |
                    Generated_Object_File  |
                    Generated_Source_Files |
                    Bound_Object_Files     |
                    Resulting_Options      |
                    Run_Path_Option =>
                  null;
            end case;
         end if;
      end if;
   end loop;

   if Main_Base_Name = null then
      Fail_Program (null, "no main base name specified");

   else
      Binder_Generated_File :=
        new String'("b__" & Main_Base_Name.all & ".adb");
   end if;

   Close (IO_File);

   --  Modify binding option -A=<file> if <file> is not an absolute path

   if Project_Paths.Last >= 1 then
      declare
         Project_Dir : constant String :=
                         Ada.Directories.Containing_Directory
                           (Project_Paths.Table (1).Path.all);
      begin
         for J in 1 .. Binding_Options_Table.Last loop
            if Binding_Options_Table.Table (J)'Length >= 4 and then
               Binding_Options_Table.Table (J) (1 .. 3) = "-A="
            then
               declare
                  File : constant String :=
                    Binding_Options_Table.Table (J)
                      (4 .. Binding_Options_Table.Table (J)'Length);
               begin
                  if not Is_Absolute_Path (File) then
                     declare
                        New_File : constant String :=
                          Normalize_Pathname (File, Project_Dir);
                     begin
                        Binding_Options_Table.Table (J) :=
                          new String'("-A=" & New_File);
                     end;
                  end if;
               end;
            end if;
         end loop;
      end;
   end if;

   --  Check if GNAT version is 6.4 or higher

   if  GNAT_Version_Set
      and then
       GNAT_Version'Length >= 2
      and then
       GNAT_Version.all /= "000"
      and then
       GNAT_Version (GNAT_Version'First .. GNAT_Version'First + 1) /= "3."
      and then
       GNAT_Version (GNAT_Version'First .. GNAT_Version'First + 1) /= "5."
   then
      GNAT_6_Or_Higher := True;

      if  GNAT_Version (GNAT_Version'First .. GNAT_Version'First + 1) /= "6."
         or else
          GNAT_Version.all >= "6.4"
      then
         GNAT_6_4_Or_Higher := True;
      end if;
   end if;

   --  Check if binding option -v was specified and issue it only if the GNAT
   --  version is 6.4 or higher, otherwise the output of gnatbind -O will be
   --  polluted.

   if Binding_Option_Dash_V_Specified and then GNAT_6_4_Or_Higher then
      Binding_Options_Table.Append (new String'("-v"));
   end if;

   if not Static_Libs then
      Add (Dash_shared, Gnatbind_Options, Last_Gnatbind_Option);
   end if;

   --  Specify the name of the generated file to gnatbind

   Add (Dash_o, Gnatbind_Options, Last_Gnatbind_Option);
   Add
     (Binder_Generated_File.all,
      Gnatbind_Options,
      Last_Gnatbind_Option);

   if not Is_Regular_File (Ada_Compiler_Path.all) then
      Fail_Program (null, "could not find the Ada compiler");
   end if;

   if Main_ALI /= null then
      Add (Main_ALI.all, Gnatbind_Options, Last_Gnatbind_Option);
   end if;

   --  If there are Stand-Alone Libraries, invoke gnatbind with -F (generate
   --  checks of elaboration flags) to avoid multiple elaborations.

   if There_Are_Stand_Alone_Libraries
     and then GNAT_Version_Set
     and then GNAT_Version'Length >= 2
     and then GNAT_Version (GNAT_Version'First .. GNAT_Version'First + 1) /=
                "3."
   then
      Add ("-F", Gnatbind_Options, Last_Gnatbind_Option);
   end if;

   for J in 1 .. ALI_Files_Table.Last loop
      Add (ALI_Files_Table.Table (J), Gnatbind_Options, Last_Gnatbind_Option);
   end loop;

   for J in 1 .. Binding_Options_Table.Last loop
      Add
        (Binding_Options_Table.Table (J),
         Gnatbind_Options,
         Last_Gnatbind_Option);

      if Binding_Options_Table.Table (J).all = Dash_OO then
         Dash_O_Specified := True;

      elsif Binding_Options_Table.Table (J)'Length >= 4 and then
            Binding_Options_Table.Table (J) (1 .. 3) = Dash_OO & '='
      then
         Dash_O_Specified := True;
         Dash_O_File_Specified := True;
         Name_Len := 0;
         Add_Str_To_Name_Buffer
           (Binding_Options_Table.Table (J)
             (4 .. Binding_Options_Table.Table (J)'Last));
         Objects_Path := Name_Find;
      end if;
   end loop;

   --  Add -x at the end, so that if -s is specified in the binding options,
   --  gnatbind does not try to look for sources, as the binder mapping file
   --  specified by -F- is not for sources, but for ALI files.

   Add (Dash_x, Gnatbind_Options, Last_Gnatbind_Option);

   if Ada_Compiler_Path = null or else
      Is_Absolute_Path (GNATBIND.all)
   then
      FULL_GNATBIND := GNATBIND;

   else
      FULL_GNATBIND :=
        new String'
              (Dir_Name (Ada_Compiler_Path.all) &
               Directory_Separator &
               GNATBIND.all);
   end if;

   if Gnatbind_Path_Specified then
      FULL_GNATBIND := Gnatbind_Path;
   end if;

   Gnatbind_Path := Locate_Exec_On_Path (FULL_GNATBIND.all);

   --  If gnatbind is not found and its full path was not specified, check for
   --  gnatbind on the path.

   if Gnatbind_Path = null and then not Gnatbind_Path_Specified then
      Gnatbind_Path := Locate_Exec_On_Path (GNATBIND.all);
   end if;

   if Gnatbind_Path = null then
      --  Make sure Namelen has a non negative value

      Name_Len := 0;

      declare
         Path_Of_Gnatbind : String_Access := GNATBIND;
      begin

         if Gnatbind_Path_Specified then
            Path_Of_Gnatbind := FULL_GNATBIND;
         end if;

         Finish_Program
           (null,
            Osint.E_Fatal,
           "could not locate " & Path_Of_Gnatbind.all);
      end;

   else
      --  Normalize the path, so that gnaampbind does not complain about not
      --  being in a "bin" directory. But don't resolve symbolic links,
      --  because in GNAT 5.01a1 and previous releases, gnatbind was a symbolic
      --  link for .gnat_wrapper.

      Gnatbind_Path :=
        new String'
          (Normalize_Pathname (Gnatbind_Path.all, Resolve_Links => False));
   end if;

   if Main_ALI = null then
      Add (No_Main_Option, Gnatbind_Options, Last_Gnatbind_Option);
   end if;

   --  Add the switch -F=<mapping file> if the mapping file was specified
   --  and the version of GNAT is recent enough.

   if Mapping_File /= null
     and then GNAT_Version_Set
     and then GNAT_Version'Length >= 2
     and then GNAT_Version (GNAT_Version'First .. GNAT_Version'First + 1) /=
                "3."
   then
      Add (Dash_Fequal & Mapping_File.all,
           Gnatbind_Options,
           Last_Gnatbind_Option);
   end if;

   --  Create temporary file to get the list of objects

   if not Dash_O_File_Specified then
      Tempdir.Create_Temp_File (FD_Objects, Objects_Path);
   end if;

   if GNAT_6_4_Or_Higher then
      if not Dash_O_File_Specified then
         Add
           (Dash_OO & "=" & Get_Name_String (Objects_Path),
            Gnatbind_Options,
            Last_Gnatbind_Option);
         Close (FD_Objects);
      end if;

   elsif not Dash_O_Specified then
      Add (Dash_OO, Gnatbind_Options, Last_Gnatbind_Option);
   end if;

   if not Quiet_Output then
      if Verbose_Low_Mode then
         Display_Last := 0;
         Add_To_Display_Line (Gnatbind_Path.all);

         for Option in 1 .. Last_Gnatbind_Option loop
            Add_To_Display_Line (Gnatbind_Options (Option).all);
         end loop;

         Put_Line (Display_Line (1 .. Display_Last));

      else
         if Main_ALI /= null then
            Display
              (Section  => GPR.Bind,
               Command  => "Ada",
               Argument => Base_Name (Main_ALI.all));

         elsif ALI_Files_Table.Last > 0 then
            Display
              (Section  => GPR.Bind,
               Command  => "Ada",
               Argument =>
                 Base_Name (ALI_Files_Table.Table (1).all) &
                 " " &
                 No_Main_Option);
         end if;
      end if;
   end if;

   declare
      Size : Natural := 0;

   begin
      for J in 1 .. Last_Gnatbind_Option loop
         Size := Size + Gnatbind_Options (J)'Length + 1;
      end loop;

      --  Invoke gnatbind with the arguments if the size is not too large or
      --  if the version of GNAT is not recent enough.

      Script_Write
        (Gnatbind_Path.all,
         Gnatbind_Options (1 .. Last_Gnatbind_Option));

      if not GNAT_6_Or_Higher or else Size <= Maximum_Size then
         if not GNAT_6_4_Or_Higher then
            Spawn
              (Gnatbind_Path.all,
               Gnatbind_Options (1 .. Last_Gnatbind_Option),
               FD_Objects,
               Return_Code,
               Err_To_Out => False);
            Success := Return_Code = 0;

         else
            Return_Code :=
              Spawn
                (Gnatbind_Path.all,
                 Gnatbind_Options (1 .. Last_Gnatbind_Option));
         end if;

      else
         --  Otherwise create a temporary response file

         declare
            FD            : File_Descriptor;
            Path          : Path_Name_Type;
            Args          : Argument_List (1 .. 1);
            EOL           : constant String (1 .. 1) := (1 => ASCII.LF);
            Status        : Integer;
            Quotes_Needed : Boolean;
            Last_Char     : Natural;
            Ch            : Character;

         begin
            Tempdir.Create_Temp_File (FD, Path);
            Args (1) := new String'("@" & Get_Name_String (Path));

            for J in 1 .. Last_Gnatbind_Option loop

               --  Check if the argument should be quoted

               Quotes_Needed := False;
               Last_Char     := Gnatbind_Options (J)'Length;

               for K in Gnatbind_Options (J)'Range loop
                  Ch := Gnatbind_Options (J) (K);

                  if Ch = ' ' or else Ch = ASCII.HT or else Ch = '"' then
                     Quotes_Needed := True;
                     exit;
                  end if;
               end loop;

               if Quotes_Needed then

                  --  Quote the argument, doubling '"'

                  declare
                     Arg : String (1 .. Gnatbind_Options (J)'Length * 2 + 2);

                  begin
                     Arg (1) := '"';
                     Last_Char := 1;

                     for K in Gnatbind_Options (J)'Range loop
                        Ch := Gnatbind_Options (J) (K);
                        Last_Char := Last_Char + 1;
                        Arg (Last_Char) := Ch;

                        if Ch = '"' then
                           Last_Char := Last_Char + 1;
                           Arg (Last_Char) := '"';
                        end if;
                     end loop;

                     Last_Char := Last_Char + 1;
                     Arg (Last_Char) := '"';

                     Status := Write (FD, Arg'Address, Last_Char);
                  end;

               else
                  Status := Write
                    (FD,
                     Gnatbind_Options (J) (Gnatbind_Options (J)'First)'Address,
                     Last_Char);
               end if;

               if Status /= Last_Char then
                  Fail_Program (null, "disk full");
               end if;

               Status := Write (FD, EOL (1)'Address, 1);

               if Status /= 1 then
                  Fail_Program (null, "disk full");
               end if;
            end loop;

            Close (FD);

            --  And invoke gnatbind with this this response file

            if not GNAT_6_4_Or_Higher then
               Spawn
                 (Gnatbind_Path.all,
                  Args,
                  FD_Objects,
                  Return_Code,
                  Err_To_Out => False);

            else
               Return_Code := Spawn (Gnatbind_Path.all, Args);
            end if;

            if Delete_Temp_Files then
               declare
                  Succ : Boolean;
                  pragma Warnings (Off, Succ);

               begin
                  Delete_File (Get_Name_String (Path), Succ);
               end;
            end if;
         end;
      end if;
   end;

   if not GNAT_6_4_Or_Higher and then not Dash_O_File_Specified then
      Close (FD_Objects);
   end if;

   if Return_Code /= 0 then
      if Delete_Temp_Files and not Dash_O_File_Specified then
         Delete_File (Get_Name_String (Objects_Path), Success);
      end if;

      Fail_Program (null, "invocation of gnatbind failed");
   end if;

   Add (Dash_c, Compiler_Options, Last_Compiler_Option);
   Add (Dash_gnatA, Compiler_Options, Last_Compiler_Option);
   Add (Dash_gnatWb, Compiler_Options, Last_Compiler_Option);
   Add (Dash_gnatiw, Compiler_Options, Last_Compiler_Option);
   Add (Dash_gnatws, Compiler_Options, Last_Compiler_Option);

   --  Read the ALI file of the first ALI file. Fetch the back end switches
   --  from this ALI file and use these switches to compile the binder
   --  generated file.

   if Main_ALI /= null or else ALI_Files_Table.Last >= 1 then
      Initialize_ALI;
      Name_Len := 0;

      if Main_ALI /= null then
         Add_Str_To_Name_Buffer (Main_ALI.all);

      else
         Add_Str_To_Name_Buffer (ALI_Files_Table.Table (1).all);
      end if;

      declare
         F : constant File_Name_Type := Name_Find;
         T : Text_Buffer_Ptr;
         A : ALI_Id;

      begin
         --  Load the ALI file

         T := Osint.Read_Library_Info (F, True);

         --  Read it. Note that we ignore errors, since we only want very
         --  limited information from the ali file, and likely a slightly
         --  wrong version will be just fine, though in normal operation
         --  we don't expect this to happen.

         A := Scan_ALI
               (F,
                T,
                Ignore_ED     => False,
                Err           => False,
                Read_Lines    => "A");

         if A /= No_ALI_Id then
            for
              Index in Units.Table (ALIs.Table (A).First_Unit).First_Arg ..
                       Units.Table (ALIs.Table (A).First_Unit).Last_Arg
            loop
               --  Do not compile with the front end switches

               declare
                  Arg : String_Access renames Args.Table (Index);
                  Argv : constant String (1 .. Arg'Length) := Arg.all;
               begin
                  if (Argv'Last <= 2 or else Argv (1 .. 2) /= "-I")
                    and then
                     (Argv'Last <= 5 or else Argv (1 .. 5) /= "-gnat")
                    and then
                     (Argv'Last <= 6 or else Argv (1 .. 6) /= "--RTS=")
                  then
                     Add
                       (String_Access (Arg),
                        Compiler_Options,
                        Last_Compiler_Option);
                  end if;
               end;
            end loop;
         end if;
      end;
   end if;

   Add (Binder_Generated_File, Compiler_Options, Last_Compiler_Option);

   declare
      Object : constant String :=
                 "b__" & Main_Base_Name.all & Ada_Object_Suffix.all;
   begin
      Add
        (Dash_o,
         Compiler_Options,
         Last_Compiler_Option);
      Add
        (Object,
         Compiler_Options,
         Last_Compiler_Option);

      --  Add the trailing options, if any

      for J in 1 .. Last_Compiler_Trailing_Option loop
         Add
           (Compiler_Trailing_Options (J),
            Compiler_Options,
            Last_Compiler_Option);
      end loop;

      if Verbose_Low_Mode then
         Name_Len := 0;

         Add_Str_To_Name_Buffer (Ada_Compiler_Path.all);

         --  Remove the executable suffix, if present

         if Executable_Suffix'Length > 0
           and then
             Name_Len > Executable_Suffix'Length
             and then
               Name_Buffer
                 (Name_Len - Executable_Suffix'Length + 1 .. Name_Len) =
               Executable_Suffix.all
         then
            Name_Len := Name_Len - Executable_Suffix'Length;
         end if;

         Display_Last := 0;
         Add_To_Display_Line (Name_Buffer (1 .. Name_Len));

         for Option in 1 .. Last_Compiler_Option loop
            Add_To_Display_Line (Compiler_Options (Option).all);
         end loop;

         Put_Line (Display_Line (1 .. Display_Last));
      end if;

      Spawn_And_Script_Write
        (Ada_Compiler_Path.all,
         Compiler_Options (1 .. Last_Compiler_Option),
         Success);

      if not Success then
         Fail_Program (null, "compilation of binder generated file failed");
      end if;

      --  Find the GCC version

      Spawn
        (Program_Name => Ada_Compiler_Path.all,
         Args         => (1 => new String'("-v")),
         Output_File  => Exchange_File_Name.all,
         Success      => Success,
         Return_Code  => Return_Code,
         Err_To_Out   => True);

      if Success then
         Open (IO_File, In_File, Exchange_File_Name.all);
         while not End_Of_File (IO_File) loop
            Get_Line (IO_File, Line, Last);

            if Last > Gcc_Version_String'Length and then
              Line (1 .. Gcc_Version_String'Length) = Gcc_Version_String
            then
               GCC_Version := Line (Gcc_Version_String'Length + 1);
               exit;
            end if;
         end loop;

         Close (IO_File);
      end if;

      Create (IO_File, Out_File, Exchange_File_Name.all);

      --  First, the generated object file

      Put_Line (IO_File, Binding_Label (Generated_Object_File));
      Put_Line (IO_File, Object);

      --  Repeat the project paths with their time stamps

      Put_Line (IO_File, Binding_Label (Project_Files));

      for J in 1 .. Project_Paths.Last loop
         Put_Line (IO_File, Project_Paths.Table (J).Path.all);
         Put_Line (IO_File, Project_Paths.Table (J).Stamp.all);
      end loop;

      --  Get the bound object files from the Object file

      Open (Objects_File, In_File, Get_Name_String (Objects_Path));

      Put_Line (IO_File, Binding_Label (Bound_Object_Files));

      while not End_Of_File (Objects_File) loop
         Get_Line (Objects_File, Line, Last);

         --  Only put in the exchange file the path of the object files.
         --  Output anything else on standard output.

         if Is_Regular_File (Line (1 .. Last)) then
            Put_Line (IO_File, Line (1 .. Last));

            Bound_Files := new Bound_File'
              (Name => new String'(Line (1 .. Last)), Next => Bound_Files);

            if Dash_O_Specified and then not Dash_O_File_Specified then
               Put_Line (Line (1 .. Last));
            end if;

         elsif not Dash_O_File_Specified then
            Put_Line (Line (1 .. Last));
         end if;
      end loop;

      Close (Objects_File);

      if Delete_Temp_Files and then not Dash_O_File_Specified then
         Delete_File (Get_Name_String (Objects_Path), Success);
      end if;

      --  For the benefit of gprclean, the generated files other than the
      --  generated object file.

      Put_Line (IO_File, Binding_Label (Generated_Source_Files));
      Put_Line (IO_File, "b__" & Main_Base_Name.all & ".ads");
      Put_Line (IO_File, Binder_Generated_File.all);
      Put_Line (IO_File, "b__" & Main_Base_Name.all & ".ali");

      --  Get the options from the binder generated file

      Open (BG_File, In_File, Binder_Generated_File.all);

      while not End_Of_File (BG_File) loop
         Get_Line (BG_File, Line, Last);
         exit when Line (1 .. Last) = Begin_Info;
      end loop;

      if not End_Of_File (BG_File) then
         Put_Line (IO_File, Binding_Label (Resulting_Options));

         All_Binding_Options := False;
         Xlinker_Seen        := False;
         Stack_Equal_Seen    := False;
         loop
            Get_Line (BG_File, Line, Last);
            exit when Line (1 .. Last) = End_Info;
            Line (1 .. Last - 8) := Line (9 .. Last);
            Last := Last - 8;

            if Line (1) = '-' then
               --  After the first switch, we take all options, because some
               --  of the options specified in pragma Linker_Options may not
               --  start with '-'.
               All_Binding_Options := True;
            end if;

            Get_Option :=
              All_Binding_Options
              or else
              (Base_Name (Line (1 .. Last)) = "g-trasym.o")
              or else
              (Base_Name (Line (1 .. Last)) = "g-trasym.obj");
            --  g-trasym is a special case as it is not included in libgnat

            --  Avoid duplication of object file

            if Get_Option then
               declare
                  BF : Bound_File_Access := Bound_Files;

               begin
                  while BF /= null loop
                     if BF.Name.all = Line (1 .. Last) then
                        Get_Option := False;
                        exit;

                     else
                        BF := BF.Next;
                     end if;
                  end loop;
               end;
            end if;

            if Get_Option then
               if Line (1 .. Last) = "-Xlinker" then
                  Xlinker_Seen := True;

               elsif Xlinker_Seen then
                  Xlinker_Seen := False;

                  --  Make sure that only the first switch --stack= is put in
                  --  the exchange file.

                  if Last > 8 and then Line (1 .. 8) = "--stack=" then
                     if not Stack_Equal_Seen then
                        Stack_Equal_Seen := True;
                        Put_Line (IO_File, "-Xlinker");
                        Put_Line (IO_File, Line (1 .. Last));
                     end if;

                  else
                     Put_Line (IO_File, "-Xlinker");
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               elsif Last > 12 and then Line (1 .. 12) = "-Wl,--stack=" then
                  if not Stack_Equal_Seen then
                     Stack_Equal_Seen := True;
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               elsif Last >= 3 and then Line (1 .. 2) = "-L" then
                  --  Set Adalib_Dir only if libgnat is found inside.
                  if Is_Regular_File
                    (Line (3 .. Last) & Directory_Separator & "libgnat.a")
                  then
                     Adalib_Dir := new String'(Line (3 .. Last));

                     if Verbose_Higher_Mode then
                        Put_Line ("Adalib_Dir = """ & Adalib_Dir.all & '"');
                     end if;

                     --  Build the Prefix_Path, where to look for some
                     --  archives: libaddr2line.a, libbfd.a, libgnatmon.a,
                     --  libgnalasup.a and libiberty.a. It contains three
                     --  directories: $(adalib)/.., $(adalib)/../.. and the
                     --  subdirectory "lib" ancestor of $(adalib).

                     declare
                        Dir_Last       : Positive;
                        Prev_Dir_Last  : Positive;
                        First          : Positive;
                        Prev_Dir_First : Positive;
                        Nmb            : Natural;
                     begin
                        Name_Len := 0;
                        Add_Str_To_Name_Buffer (Line (3 .. Last));

                        while Name_Buffer (Name_Len) = Directory_Separator
                          or else Name_Buffer (Name_Len) = '/'
                        loop
                           Name_Len := Name_Len - 1;
                        end loop;

                        while Name_Buffer (Name_Len) /= Directory_Separator
                          and then Name_Buffer (Name_Len) /= '/'
                        loop
                           Name_Len := Name_Len - 1;
                        end loop;

                        while Name_Buffer (Name_Len) = Directory_Separator
                          or else Name_Buffer (Name_Len) = '/'
                        loop
                           Name_Len := Name_Len - 1;
                        end loop;

                        Dir_Last := Name_Len;
                        Nmb := 0;

                        Dir_Loop : loop
                           Prev_Dir_Last := Dir_Last;
                           First := Dir_Last - 1;
                           while First > 3
                             and then
                              Name_Buffer (First) /= Directory_Separator
                             and then
                              Name_Buffer (First) /= '/'
                           loop
                              First := First - 1;
                           end loop;

                           Prev_Dir_First := First + 1;

                           exit Dir_Loop when First <= 3;

                           Dir_Last := First - 1;
                           while Name_Buffer (Dir_Last) = Directory_Separator
                             or else Name_Buffer (Dir_Last) = '/'
                           loop
                              Dir_Last := Dir_Last - 1;
                           end loop;

                           Nmb := Nmb + 1;

                           if Nmb <= 1 then
                              Add_Char_To_Name_Buffer (Path_Separator);
                              Add_Str_To_Name_Buffer
                                (Name_Buffer (1 .. Dir_Last));

                           elsif Name_Buffer (Prev_Dir_First .. Prev_Dir_Last)
                             = "lib"
                           then
                              Add_Char_To_Name_Buffer (Path_Separator);
                              Add_Str_To_Name_Buffer
                                (Name_Buffer (1 .. Prev_Dir_Last));
                              exit Dir_Loop;
                           end if;
                        end loop Dir_Loop;

                        Prefix_Path :=
                          new String'(Name_Buffer (1 .. Name_Len));

                        if Verbose_Higher_Mode then
                           Put_Line
                             ("Prefix_Path = """ & Prefix_Path.all & '"');
                        end if;
                     end;
                  end if;
                  Put_Line (IO_File, Line (1 .. Last));

               elsif Line (1 .. Last) = Static_Libgcc then
                  Put_Line (IO_File, Line (1 .. Last));
                  Libgcc_Specified := True;

               elsif Line (1 .. Last) = Shared_Libgcc then
                  Put_Line (IO_File, Line (1 .. Last));
                  Libgcc_Specified := True;

               elsif Line (1 .. Last) = "-static" then
                  Static_Libs := True;
                  Put_Line (IO_File, Line (1 .. Last));

                  if Shared_Libgcc_Default = 'T'
                    and then GCC_Version >= '3'
                    and then not Libgcc_Specified
                  then
                     Put_Line (IO_File, Static_Libgcc);
                  end if;

               elsif Line (1 .. Last) = "-shared" then
                  Static_Libs := False;
                  Put_Line (IO_File, Line (1 .. Last));

                  if GCC_Version >= '3'
                    and then not Libgcc_Specified
                  then
                     Put_Line (IO_File, Shared_Libgcc);
                  end if;

                  --  For a number of archives, we need to indicate the full
                  --  path of the archive, if we find it, to be sure that the
                  --  correct archive is used by the linker.

               elsif Line (1 .. Last) = "-lgnat" then
                  if Adalib_Dir = null then
                     if Verbose_Higher_Mode then
                        Put_Line ("No Adalib_Dir");
                     end if;

                     Put_Line (IO_File, "-lgnat");

                  elsif Static_Libs then
                     Put_Line (IO_File, Adalib_Dir.all & "libgnat.a");

                  else
                     Put_Line (IO_File, "-lgnat");
                  end if;

               elsif Line (1 .. Last) = "-lgnarl" and then
                     Static_Libs and then
                     Adalib_Dir /= null
               then
                  Put_Line (IO_File, Adalib_Dir.all & "libgnarl.a");

               elsif Line (1 .. Last) = "-laddr2line"
                 and then Prefix_Path /= null
               then
                  Lib_Path := Locate_Regular_File
                    ("libaddr2line.a", Prefix_Path.all);

                  if Lib_Path /= null then
                     Put_Line (IO_File, Lib_Path.all);
                     Free (Lib_Path);

                  else
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               elsif Line (1 .. Last) = "-lbfd"
                 and then Prefix_Path /= null
               then
                  Lib_Path := Locate_Regular_File
                    ("libbfd.a", Prefix_Path.all);

                  if Lib_Path /= null then
                     Put_Line (IO_File, Lib_Path.all);
                     Free (Lib_Path);

                  else
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               elsif Line (1 .. Last) = "-lgnalasup"
                 and then Prefix_Path /= null
               then
                  Lib_Path := Locate_Regular_File
                    ("libgnalasup.a", Prefix_Path.all);

                  if Lib_Path /= null then
                     Put_Line (IO_File, Lib_Path.all);
                     Free (Lib_Path);

                  else
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               elsif Line (1 .. Last) = "-lgnatmon"
                 and then Prefix_Path /= null
               then
                  Lib_Path := Locate_Regular_File
                    ("libgnatmon.a", Prefix_Path.all);

                  if Lib_Path /= null then
                     Put_Line (IO_File, Lib_Path.all);
                     Free (Lib_Path);

                  else
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               elsif Line (1 .. Last) = "-liberty"
                 and then Prefix_Path /= null
               then
                  Lib_Path := Locate_Regular_File
                    ("libiberty.a", Prefix_Path.all);

                  if Lib_Path /= null then
                     Put_Line (IO_File, Lib_Path.all);
                     Free (Lib_Path);

                  else
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               else
                  Put_Line (IO_File, Line (1 .. Last));
               end if;
            end if;
         end loop;
      end if;

      Close (BG_File);

      if not Static_Libs
        and then Adalib_Dir /= null
      then
         Put_Line (IO_File, Binding_Label (Run_Path_Option));
         Put_Line (IO_File, Adalib_Dir.all);
         Name_Len := Adalib_Dir'Length;
         Name_Buffer (1 .. Name_Len) := Adalib_Dir.all;

         for J in reverse 2 .. Name_Len - 4 loop
            if Name_Buffer (J) = Directory_Separator and then
              Name_Buffer (J + 4) = Directory_Separator and then
              Name_Buffer (J + 1 .. J + 3) = "lib"
            then
               Name_Len := J + 3;
               Put_Line (IO_File, Name_Buffer (1 .. Name_Len));
               exit;
            end if;
         end loop;
      end if;

      Close (IO_File);
   end;
end Gprbind;
