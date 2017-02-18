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

--  This is the version of the body of procedure Build_Shared_Lib for most
--  where shared libraries are supported.

separate (Gprlib)
procedure Build_Shared_Lib is

   Ofiles : constant Argument_List :=
               Argument_List (Object_Files.Table (1 .. Object_Files.Last));

   Options : constant Argument_List :=
               Argument_List (Options_Table.Table (1 .. Options_Table.Last));

   Lib_File : constant String :=
                Shared_Lib_Prefix.all &
                Library_Name.all & Shared_Lib_Suffix.all;

   Lib_Path : constant String :=
                Library_Directory.all & Lib_File;

   Maj_Version : String_Access := new String'("");

   Result  : Integer;
   pragma Unreferenced (Result);

   procedure Build (Output_File : String);
   --  Find the library builder executable and invoke it with the correct
   --  options to build the shared library.

   -----------
   -- Build --
   -----------

   procedure Build (Output_File : String) is
      Success  : Boolean;

      Out_Opt : constant String_Access := new String'("-o");
      Out_V   : constant String_Access := new String'(Output_File);

      Driver   : String_Access;

      Response_File_Name : Path_Name_Type := No_Path;
      Response_2         : Path_Name_Type := No_Path;
      Export_File        : Path_Name_Type := No_Path;

      procedure Display_Linking_Command;
      --  Display the linking command, depending on verbosity and quiet output

      -----------------------------
      -- Display_Linking_Command --
      -----------------------------

      procedure Display_Linking_Command is
      begin
         if not Opt.Quiet_Output then
            if Opt.Verbose_Mode then
               Name_Len := 0;

               Add_Str_To_Name_Buffer (Driver.all);

               for J in 1 .. Last_Arg loop
                  Add_Str_To_Name_Buffer (" ");
                  Add_Str_To_Name_Buffer (Arguments (J).all);
               end loop;

               Put_Line (Name_Buffer (1 .. Name_Len));

            else
               Display
                 (Section  => Build_Libraries,
                  Command  => "link library",
                  Argument => Lib_File);
            end if;
         end if;
      end Display_Linking_Command;

   begin
      --  Get the executable to use, either the specified Driver, or "gcc"

      if Driver_Name = No_Name then
         Driver := Locate_Exec_On_Path (Gcc_Name);

         if Driver = null then
            Fail_Program (null, Gcc_Name & " not found in path");
         end if;

      else
         Driver := Locate_Exec_On_Path (Get_Name_String (Driver_Name));

         if Driver = null then
            Fail_Program
              (null, Get_Name_String (Driver_Name) & " not found in path");
         end if;
      end if;

      Last_Arg := 0;
      Argument_Length := Driver'Length;

      --  The minimum arguments

      for J in 1 .. Shared_Lib_Minimum_Options.Last loop
         Add_Arg (Shared_Lib_Minimum_Options.Table (J));
      end loop;

      --  The leading library options, if any

      for J in 1 .. Leading_Library_Options_Table.Last loop
         Add_Arg (Leading_Library_Options_Table.Table (J));
      end loop;

      --  -o <library file name>

      Add_Arg (Out_Opt);
      Add_Arg (Out_V);

      --  The options

      for J in Options'Range loop
         if Options (J) /= null and then Options (J).all /= "" then
            Add_Arg (Options (J));
         end if;
      end loop;

      --  Other options

      for J in 1 .. Library_Version_Options.Last loop
         if Library_Version_Options.Table (J).all /= "" then
            Add_Arg (Library_Version_Options.Table (J));
         end if;
      end loop;

      --  The object files

      if Partial_Linker /= null then
         Partial_Linker_Path := Locate_Exec_On_Path (Partial_Linker.all);

         if Partial_Linker_Path = null then
            Fail_Program
              (null, "unable to locate linker " & Partial_Linker.all);
         end if;
      end if;

      if Resp_File_Format = GPR.None
        and then Partial_Linker_Path /= null
      then
         --  If partial linker is used, do a partial link first

         Partial_Number := 0;
         First_Object := Ofiles'First;

         loop
            declare
               Partial : constant String_Access :=
                           new String'
                             (Partial_Name
                                (Library_Name.all,
                                 Partial_Number,
                                 Object_Suffix));
               Size    : Natural := 0;

               Saved_Last_PL_Option : Natural;
            begin
               Saved_Last_PL_Option := Last_PL_Option;

               Add (Partial, PL_Options, Last_PL_Option);
               Size := Size + 1 + Partial'Length;

               if Partial_Number > 0 then
                  Add
                    (Partial_Name
                       (Library_Name.all,
                        Partial_Number - 1,
                        Object_Suffix),
                     PL_Options,
                     Last_PL_Option);
               end if;

               for J in 1 .. Last_PL_Option loop
                  Size := Size + 1 + PL_Options (J)'Length;
               end loop;

               loop
                  Add
                    (Ofiles (First_Object),
                     PL_Options,
                     Last_PL_Option);
                  Size := Size + 1 + PL_Options (Last_PL_Option)'Length;

                  First_Object := First_Object + 1;
                  exit when
                    First_Object > Ofiles'Last or else
                    Size >= Maximum_Size;
               end loop;

               if not Quiet_Output then
                  if Verbose_Mode then
                     Add_Str_To_Name_Buffer (Partial_Linker_Path.all);

                     for J in 1 .. Last_PL_Option loop
                        Add_Str_To_Name_Buffer (" ");
                        Add_Str_To_Name_Buffer (PL_Options (J).all);
                     end loop;

                     Put_Line (Name_Buffer (1 .. Name_Len));
                  end if;
               end if;

               Spawn_And_Script_Write
                 (Partial_Linker_Path.all,
                  PL_Options (1 .. Last_PL_Option),
                  Success);

               if not Success then
                  Fail_Program
                    (null,
                     "call to linker driver " &
                     Partial_Linker.all & " failed");
               end if;

               if First_Object > Ofiles'Last then
                  Add_Arg (Partial);
                  exit;
               end if;

               Last_PL_Option := Saved_Last_PL_Option;
               Partial_Number := Partial_Number + 1;
            end;
         end loop;

      else
         First_Object := Last_Arg + 1;

         for J in Ofiles'Range loop
            Add_Arg (Ofiles (J));
         end loop;
      end if;

      Last_Object := Last_Arg;

      --  In Ofiles we can have at the end some libraries -lname, so ensure
      --  that the object are only taken up to Last_Object_File_Index.

      if Last_Object_File_Index > First_Object
        and then Last_Object_File_Index < Last_Object
      then
         Last_Object := Last_Object_File_Index;
      end if;

      --  Finally the additional switches, the library switches and the library
      --  options.

      for J in 1 .. Additional_Switches.Last loop
         Add_Arg (Additional_Switches.Table (J));
      end loop;

      for J in 1 .. Library_Switches_Table.Last loop
         Add_Arg (Library_Switches_Table.Table (J));
      end loop;

      for J in 1 .. Library_Options_Table.Last loop
         Add_Arg (Library_Options_Table.Table (J));
      end loop;

      --  Check if a response file is needed

      if Max_Command_Line_Length > 0
        and then Argument_Length > Max_Command_Line_Length
        and then Resp_File_Format /= GPR.None
      then
         declare
            --  Preserve the options, if any

            Options : constant String_List :=
                        Arguments (Last_Object + 1 .. Last_Arg);

         begin
            Create_Response_File
              (Format            => Resp_File_Format,
               Objects           => Arguments (First_Object .. Last_Object),
               Other_Arguments   => Options,
               Resp_File_Options => Response_File_Switches.all,
               Name_1            => Response_File_Name,
               Name_2            => Response_2);

            Record_Temp_File
              (Shared => null,
               Path   => Response_File_Name);

            if Response_2 /= No_Path then
               Record_Temp_File
                 (Shared => null,
                  Path   => Response_2);
            end if;

            Last_Arg := First_Object - 1;

            if Resp_File_Format = GCC
              or else Resp_File_Format = GCC_GNU
              or else Resp_File_Format = GCC_Object_List
              or else Resp_File_Format = GCC_Option_List
            then
               Add_Arg
                 (new String'("@" & Get_Name_String (Response_File_Name)));
            else
               if Response_File_Switches'Length /= 0 then
                  for J in Response_File_Switches'First ..
                    Response_File_Switches'Last - 1
                  loop
                     Add_Arg (Response_File_Switches (J));
                  end loop;

                  Add_Arg
                    (new String'
                       (Response_File_Switches
                          (Response_File_Switches'Last).all &
                        Get_Name_String (Response_File_Name)));

               else
                  Add_Arg
                    (new String'(Get_Name_String (Response_File_Name)));
               end if;

               --  Put back the options

               for J in Options'Range loop
                  Add_Arg (Options (J));
               end loop;
            end if;
         end;
      end if;

      --  For a standalone shared library, create an export symbols file if
      --  supported. We need a support for an export file and either:
      --
      --  A library symbol file to be defined
      --    or
      --  An object lister and the corresponding matcher

      if Standalone /= No and then Export_File_Switch /= null then
         if Library_Symbol_File /= null then
            --  The exported symbols are to be taken from the symbol file

            Create_Export_Symbols_File
              (Driver_Path         => "",
               Options             => OL_Options (1 .. Last_OL_Option),
               Sym_Matcher         => "",
               Format              => Export_File_Format,
               Objects             => String_List'(1 .. 0 => null),
               Library_Symbol_File => Library_Symbol_File.all,
               Export_File_Name    => Export_File);

         elsif Object_Lister /= null
           and then Object_Lister_Matcher /= null
         then
            --  The exported symbols are to be read from the object artifacts
            --  of the library interface.

            declare
               List : String_List
                 (1 .. Interface_Objs.Last + Generated_Objects.Last);
            begin
               --  Ada unit interfaces

               for K in 1 .. Interface_Objs.Last loop
                  List (K) := Interface_Objs.Table (K);
               end loop;

               --  We need to add the binder generated object file which
               --  contains the library initilization code to be explicitely
               --  called by the main application.

               for K in 1 .. Generated_Objects.Last loop
                  List (Interface_Objs.Last + K) :=
                    Generated_Objects.Table (K);
               end loop;

               Create_Export_Symbols_File
                 (Driver_Path         => Object_Lister.all,
                  Options             => OL_Options (1 .. Last_OL_Option),
                  Sym_Matcher         => Object_Lister_Matcher.all,
                  Format              => Export_File_Format,
                  Objects             => List,
                  Library_Symbol_File => "",
                  Export_File_Name    => Export_File);
            end;
         end if;

         --  If the export file has been created properly pass it to the linker

         if Export_File /= No_Path then
            Add_Arg
              (new String'(Export_File_Switch.all
               & Get_Name_String (Export_File)));
         end if;
      end if;

      Display_Linking_Command;

      --  Finally spawn the library builder driver

      Spawn_And_Script_Write (Driver.all, Arguments (1 .. Last_Arg), Success);

      if not Success then
         if Driver_Name = No_Name then
            Fail_Program (null, Gcc_Name & " execution error");

         else
            Fail_Program
              (null, Get_Name_String (Driver_Name) & " execution error");
         end if;
      end if;
   end Build;

--  Start of processing for Build_Shared_Lib

begin
   if Verbosity_Level > Opt.Low then
      Put ("building relocatable shared library ");
      Put_Line (Lib_File);
   end if;

   if Library_Version.all = "" or else not Symbolic_Link_Supported then
      --  If no Library_Version specified, make sure the table is empty and
      --  call Build.

      Library_Version_Options.Set_Last (0);
      Build (Lib_Path);

   else
      --  Put the necessary options corresponding to the Library_Version in the
      --  table.

      if Major_Minor_Id_Supported then
         Maj_Version :=
           new String'(Major_Id_Name (Lib_File, Library_Version.all));
      end if;

      if Library_Version_Options.Last > 0 then
         if Maj_Version.all /= "" then
            Library_Version_Options.Table (Library_Version_Options.Last)  :=
              new String'
                (Library_Version_Options.Table
                     (Library_Version_Options.Last).all & Maj_Version.all);

         else
            Library_Version_Options.Table (Library_Version_Options.Last)  :=
              new String'
                (Library_Version_Options.Table
                     (Library_Version_Options.Last).all &
                 Library_Version.all);
         end if;
      end if;

      if Is_Absolute_Path (Library_Version.all) then
         Library_Version_Path := Library_Version;

      else
         Library_Version_Path :=
           new String'
             (Library_Directory.all & Library_Version.all);
      end if;

      --  Now that the table has been filled, call Build

      Build (Library_Version_Path.all);

      --  Create symbolic link, if appropriate

      if Library_Version.all /= Lib_Path then
         Create_Sym_Links
           (Lib_Path,
            Library_Version.all,
            Library_Directory.all,
            Maj_Version.all);
      end if;

   end if;
end Build_Shared_Lib;
