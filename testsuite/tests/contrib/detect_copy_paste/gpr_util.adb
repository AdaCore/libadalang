------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2007-2016, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Calendar.Time_Zones;   use Ada.Calendar; use Ada.Calendar.Time_Zones;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Directories;           use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Streams.Stream_IO;     use Ada.Streams;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Dynamic_HTables;      use GNAT.Dynamic_HTables;
with GNAT.IO_Aux;
with GNAT.Sockets;
with GNAT.Table;
with GNAT.Regpat;               use GNAT.Regpat;

with Interfaces.C.Strings;
with System;

with Gpr_Build_Util;     use Gpr_Build_Util;
with GprConfig.Sdefault;
with GPR_Version;        use GPR_Version;
with GPR.ALI;            use GPR.ALI;
with GPR.Com;
with GPR.Debug;
with GPR.Opt;            use GPR.Opt;
with GPR.Osint;          use GPR.Osint;
with GPR.Conf;
with GPR.Env;
with GPR.Err;
with GPR.Names;          use GPR.Names;
with GPR.Scans;
with GPR.Sinput;
with GPR.Tempdir;
with GPR.Util;           use GPR.Util;

package body Gpr_Util is

   use GPR.Stamps;

   Libgcc_Subdir_Ptr : Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Libgcc_Subdir_Ptr, "__gnat_default_libgcc_subdir");
   --  Pointer to string indicating the installation subdirectory where a
   --  default shared libgcc might be found.

   GNU_Header  : aliased constant String := "INPUT (";
   GNU_Opening : aliased constant String := """";
   GNU_Closing : aliased constant String := '"' & ASCII.LF;
   GNU_Footer  : aliased constant String := ')' & ASCII.LF;

   package Project_Name_Boolean_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");

   Project_Failure : Project_Name_Boolean_Htable.Instance :=
                       Project_Name_Boolean_Htable.Nil;
   --  Record a boolean for project having failed to compile cleanly

   -------------------------------
   -- Binder_Exchange_File_Name --
   -------------------------------

   function Binder_Exchange_File_Name
     (Main_Base_Name : File_Name_Type; Prefix : Name_Id) return String_Access
   is
      File_Name : constant String := Get_Name_String (Main_Base_Name);
   begin
      Get_Name_String (Prefix);
      Add_Str_To_Name_Buffer (File_Name);
      Add_Str_To_Name_Buffer (Binder_Exchange_Suffix);
      return new String'(Name_Buffer (1 .. Name_Len));
   end Binder_Exchange_File_Name;

   -----------------------
   -- Compute_Slave_Env --
   -----------------------

   function Compute_Slave_Env
     (Project : Project_Tree_Ref; Auto : Boolean) return String
   is
      User      : String_Access := Getenv ("USER");
      User_Name : String_Access := Getenv ("USERNAME");
      Default   : constant String :=
                    (if User = null
                     then (if User_Name = null
                       then "unknown" else User_Name.all)
                     else User.all)
                    & '@' & GNAT.Sockets.Host_Name;

      package S_Set is new Containers.Indefinite_Ordered_Sets (String);

      Set : S_Set.Set;
      Ctx : Context;

   begin
      Free (User);
      Free (User_Name);

      if Auto then
         --  In this mode the slave environment is computed based on
         --  the project variable value and the command line arguments.

         --  First adds all command line arguments

         for K in 1 .. Argument_Count loop
            --  Skip arguments that are not changing the actual compilation and
            --  this will ensure that the same environment will be created for
            --  gprclean.

            if Argument (K) not in "-p" | "-d" | "-c" | "-q"
              and then
                (Argument (K)'Length < 2
                 or else Argument (K) (1 .. 2) /= "-j")
            then
               Set.Insert (Argument (K));
            end if;
         end loop;

         --  Then all the global variables for the project tree

         for K in
           1 .. Variable_Element_Table.Last (Project.Shared.Variable_Elements)
         loop
            declare
               V : constant Variable :=
                     Project.Shared.Variable_Elements.Table (K);
            begin
               if V.Value.Kind = Single then
                  Set.Include
                    (Get_Name_String (V.Name)
                     & "=" & Get_Name_String (V.Value.Value));
               end if;
            end;
         end loop;

         --  Compute the MD5 sum of the sorted elements in the set

         for S of Set loop
            Update (Ctx, S);
         end loop;

         return Default & "-" & Digest (Ctx);

      else
         --  Otherwise use the default <user_name> & '@' & <host_name>
         return Default;
      end if;
   end Compute_Slave_Env;

   ------------------------------
   -- Check_Version_And_Help_G --
   ------------------------------

   --  Common switches for GNU tools

   Version_Switch : constant String := "--version";
   Help_Switch    : constant String := "--help";

   procedure Check_Version_And_Help_G
     (Tool_Name      : String;
      Initial_Year   : String;
      Version_String : String)
   is
      Version_Switch_Present : Boolean := False;
      Help_Switch_Present    : Boolean := False;
      Next_Arg               : Natural;

   begin
      --  First check for --version or --help

      Next_Arg := 1;
      while Next_Arg <= Argument_Count loop
         declare
            Next_Argv : constant String := Argument (Next_Arg);
         begin
            if Next_Argv = Version_Switch then
               Version_Switch_Present := True;

            elsif Next_Argv = Help_Switch then
               Help_Switch_Present := True;
            end if;

            Next_Arg := Next_Arg + 1;
         end;
      end loop;

      --  If --version was used, display version and exit

      if Version_Switch_Present then
         Display_Version (Tool_Name, Initial_Year, Version_String);

         Put_Line (Free_Software);
         New_Line;

         OS_Exit (0);
      end if;

      --  If --help was used, display help and exit

      if Help_Switch_Present then
         Usage;
         New_Line;
         Put_Line ("Report bugs to report@adacore.com");
         OS_Exit (0);
      end if;
   end Check_Version_And_Help_G;

   --------------------------------
   -- Create_Export_Symbols_File --
   --------------------------------

   procedure Create_Export_Symbols_File
     (Driver_Path         : String;
      Options             : Argument_List;
      Sym_Matcher         : String;
      Format              : Export_File_Format;
      Objects             : String_List;
      Library_Symbol_File : String;
      Export_File_Name    : out Path_Name_Type)
   is
      use type Containers.Count_Type;

      package Syms_List is new Containers.Indefinite_Ordered_Sets (String);

      procedure Get_Syms (Object_File : String);
      --  Read exported symbols from Object_File and add them into Syms

      procedure Write (Str : String);
      --  Write Str into the export file

      Pattern : constant Pattern_Matcher := Compile (Sym_Matcher);

      Syms : Syms_List.Set;
      FD   : File_Descriptor;

      --------------
      -- Get_Syms --
      --------------

      procedure Get_Syms (Object_File : String) is
         Success   : Boolean;
         Ret       : Integer;
         Opts      : Argument_List (1 .. Options'Length + 1);
         File      : File_Type;
         File_Name : Path_Name_Type;
         Matches   : Match_Array (0 .. 1);

         function Filename return String is (Get_Name_String (File_Name));
         --  Remove the ASCII.NUL from end of temporary file-name

      begin
         Opts (1 .. Options'Length) := Options;
         Opts (Opts'Last) := new String'(Object_File);

         GPR.Tempdir.Create_Temp_File (FD, File_Name);
         Record_Temp_File (null, File_Name);

         Close (FD);

         if Verbose_Mode then
            Put  (Driver_Path);
            for O of Opts loop
               Put (' ');
               Put (O.all);
            end loop;
            New_Line;
         end if;

         Spawn (Driver_Path, Opts, Filename, Success, Ret);

         if Success then
            Open (File, In_File, Filename);

            while not End_Of_File (File) loop
               declare
                  use GNAT;
                  Buffer : constant String := IO_Aux.Get_Line (File);
               begin
                  Match (Pattern, Buffer, Matches);

                  if Matches (1) /= No_Match then
                     Syms.Include
                       (Buffer (Matches (1).First .. Matches (1).Last));
                  end if;
               end;
            end loop;

            Close (File);
         end if;

         Free (Opts (Opts'Last));
      end Get_Syms;

      -----------
      -- Write --
      -----------

      procedure Write (Str : String) is
         S : constant String := Str & ASCII.LF;
         R : Integer with Unreferenced;
      begin
         R := Write (FD, S (S'First)'Address, S'Length);
      end Write;

   begin
      Export_File_Name := No_Path;

      if Format = None then
         return;
      end if;

      if Library_Symbol_File = "" then
         --  Get the exported symbols from every object files, first get the nm
         --  tool for the target.

         for K in Objects'Range loop
            Get_Syms (Objects (K).all);
         end loop;

      else
         --  Get the symbols from the symbol file, one symbol per line

         if Is_Readable_File (Library_Symbol_File) then
            declare
               File : File_Type;
               Line : String (1 .. 1_024);
               Last : Natural;
            begin
               Open (File, In_File, Library_Symbol_File);

               while not End_Of_File (File) loop
                  Get_Line (File, Line, Last);

                  if Last > 0 then
                     Syms.Include (Line (1 .. Last));
                  end if;
               end loop;

               Close (File);
            end;

         else
            raise Constraint_Error
              with "unable to locate Library_Symbol_File"""
                   & Library_Symbol_File & '"';
         end if;
      end if;

      if Syms.Length = 0 then
         return;
      end if;

      --  Now create the export file, either GNU or DEF format

      Create_Export_File : declare
         File_Name : Path_Name_Type;
      begin
         --  Create (Export_File, Out_File);

         GPR.Tempdir.Create_Temp_File (FD, File_Name);
         Record_Temp_File (null, File_Name);

         Get_Name_String (File_Name);

         --  Always add .def at the end, this is needed for Windows

         Add_Str_To_Name_Buffer (".def");
         Export_File_Name := Name_Find;
         Record_Temp_File (null, Export_File_Name);

         --  Header

         case Format is
            when GNU =>
               Write ("SYMS {");
               Write ("   global:");

            when Def =>
               Write ("EXPORTS");

            when None | Flat =>
               null;
         end case;

         --  Symbols

         for Sym of Syms loop
            case Format is
               when GNU =>
                  Write (Sym & ";");

               when Def | Flat =>
                  Write (Sym);

               when None =>
                  null;
            end case;
         end loop;

         --  Footer

         case Format is
            when GNU =>
               Write ("   local: *;");
               Write ("};");

            when None | Def | Flat =>
               null;
         end case;

         Close (FD);

         Copy_File
           (Get_Name_String (File_Name),
            Get_Name_String (Export_File_Name),
            Success);
      end Create_Export_File;
   end Create_Export_Symbols_File;

   --------------------------
   -- Create_Response_File --
   --------------------------

   procedure Create_Response_File
     (Format            : Response_File_Format;
      Objects           : String_List;
      Other_Arguments   : String_List;
      Resp_File_Options : String_List;
      Name_1            : out Path_Name_Type;
      Name_2            : out Path_Name_Type)
   is
      Resp_File : File_Descriptor;
      Status    : Integer;
      pragma Warnings (Off, Status);
      Closing_Status : Boolean;
      pragma Warnings (Off, Closing_Status);

      function Modified_Argument (Arg : String) return String;
      --  If the argument includes a space, a backslash, or a double quote,
      --  escape the character with a preceding backsash.

      -----------------------
      -- Modified_Argument --
      -----------------------

      function Modified_Argument (Arg : String) return String is
         Result : String (1 .. 2 * Arg'Length);
         Last   : Natural := 0;

         procedure Add (C : Character);

         ---------
         -- Add --
         ---------

         procedure Add (C : Character) is
         begin
            Last := Last + 1;
            Result (Last) := C;
         end Add;

      begin
         for J in Arg'Range loop
            if Arg (J) = '\' or else Arg (J) = ' ' or else Arg (J) = '"' then
               Add ('\');
            end if;

            Add (Arg (J));
         end loop;

         return Result (1 .. Last);
      end Modified_Argument;

   begin
      Name_2 := No_Path;
      Tempdir.Create_Temp_File (Resp_File, Name => Name_1);
      Record_Temp_File (null, Name_1);

      if Format = GNU or else Format = GCC_GNU then
         Status := Write (Resp_File, GNU_Header'Address, GNU_Header'Length);
      end if;

      for J in Objects'Range loop
         if Format = GNU or else Format = GCC_GNU then
            Status :=
              Write (Resp_File, GNU_Opening'Address, GNU_Opening'Length);
         end if;

         Status :=
           Write (Resp_File, Objects (J).all'Address, Objects (J)'Length);

         if Format = GNU or else Format = GCC_GNU then
            Status :=
              Write (Resp_File, GNU_Closing'Address, GNU_Closing'Length);

         else
            Status :=
              Write (Resp_File, ASCII.LF'Address, 1);
         end if;
      end loop;

      if Format = GNU or else Format = GCC_GNU then
         Status := Write (Resp_File, GNU_Footer'Address, GNU_Footer'Length);
      end if;

      case Format is
         when GCC_GNU | GCC_Object_List | GCC_Option_List =>
            Close (Resp_File, Closing_Status);
            Name_2 := Name_1;
            Tempdir.Create_Temp_File (Resp_File, Name => Name_1);
            Record_Temp_File (null, Name_1);

            declare
               Arg : constant String :=
                       Modified_Argument (Get_Name_String (Name_2));

            begin
               for J in Resp_File_Options'Range loop
                  Status :=
                    Write
                      (Resp_File,
                       Resp_File_Options (J) (1)'Address,
                       Resp_File_Options (J)'Length);

                  if J < Resp_File_Options'Last then
                     Status := Write (Resp_File, ASCII.LF'Address, 1);
                  end if;
               end loop;

               Status := Write (Resp_File, Arg (1)'Address, Arg'Length);
            end;

            Status := Write (Resp_File, ASCII.LF'Address, 1);

         when GCC =>
            null;

         when others =>
            Close (Resp_File, Closing_Status);
      end case;

      if        Format = GCC
        or else Format = GCC_GNU
        or else Format = GCC_Object_List
        or else Format = GCC_Option_List
      then
         for J in Other_Arguments'Range loop
            declare
               Arg : constant String :=
                       Modified_Argument (Other_Arguments (J).all);

            begin
               Status := Write (Resp_File, Arg (1)'Address, Arg'Length);
            end;

            Status := Write (Resp_File, ASCII.LF'Address, 1);
         end loop;

         Close (Resp_File, Closing_Status);
      end if;
   end Create_Response_File;

   ---------------------
   -- Create_Sym_Link --
   ---------------------

   procedure Create_Sym_Link (From, To : String) is

      function Symlink
        (Oldpath : System.Address;
         Newpath : System.Address) return Integer;
      pragma Import (C, Symlink, "__gnat_symlink");

      C_From  : constant String := From & ASCII.NUL;
      C_To    : constant String :=
                  Relative_Path
                    (Containing_Directory (To), Containing_Directory (From))
                  & Ada.Directories.Simple_Name (To) & ASCII.NUL;
      Result  : Integer;
      Success : Boolean;
      pragma Unreferenced (Success, Result);

   begin
      Delete_File (From, Success);
      Result := Symlink (C_To'Address, C_From'Address);
   end Create_Sym_Link;

   ----------------------
   -- Create_Sym_Links --
   ----------------------

   procedure Create_Sym_Links
     (Lib_Path    : String;
      Lib_Version : String;
      Lib_Dir     : String;
      Maj_Version : String)
   is
      function Symlink
        (Oldpath : System.Address;
         Newpath : System.Address) return Integer;
      pragma Import (C, Symlink, "__gnat_symlink");

      Version_Path : String_Access;

      Success : Boolean;
      Result  : Integer;
      pragma Unreferenced (Success, Result);

   begin
      Version_Path := new String (1 .. Lib_Version'Length + 1);
      Version_Path (1 .. Lib_Version'Length) := Lib_Version;
      Version_Path (Version_Path'Last)       := ASCII.NUL;

      if Maj_Version'Length = 0 then
         declare
            Newpath : String (1 .. Lib_Path'Length + 1);
         begin
            Newpath (1 .. Lib_Path'Length) := Lib_Path;
            Newpath (Newpath'Last)         := ASCII.NUL;
            Delete_File (Lib_Path, Success);
            Result := Symlink (Version_Path (1)'Address, Newpath'Address);
         end;

      else
         declare
            Newpath1 : String (1 .. Lib_Path'Length + 1);
            Maj_Path : constant String :=
                         Lib_Dir & Directory_Separator & Maj_Version;
            Newpath2 : String (1 .. Maj_Path'Length + 1);
            Maj_Ver  : String (1 .. Maj_Version'Length + 1);

         begin
            Newpath1 (1 .. Lib_Path'Length) := Lib_Path;
            Newpath1 (Newpath1'Last)        := ASCII.NUL;

            Newpath2 (1 .. Maj_Path'Length) := Maj_Path;
            Newpath2 (Newpath2'Last)        := ASCII.NUL;

            Maj_Ver (1 .. Maj_Version'Length) := Maj_Version;
            Maj_Ver (Maj_Ver'Last)            := ASCII.NUL;

            Delete_File (Maj_Path, Success);

            Result := Symlink (Version_Path (1)'Address, Newpath2'Address);

            Delete_File (Lib_Path, Success);

            Result := Symlink (Maj_Ver'Address, Newpath1'Address);
         end;
      end if;
   end Create_Sym_Links;

   ------------------------------------
   -- Display_Usage_Version_And_Help --
   ------------------------------------

   procedure Display_Usage_Version_And_Help is
   begin
      Put_Line ("  --version   Display version and exit");
      Put_Line ("  --help      Display usage and exit");
      New_Line;
   end Display_Usage_Version_And_Help;

   ---------------------
   -- Display_Version --
   ---------------------

   procedure Display_Version
     (Tool_Name      : String;
      Initial_Year   : String;
      Version_String : String)
   is
   begin
      Put_Line (Tool_Name & " " & Version_String);

      Put ("Copyright (C) ");
      Put (Initial_Year);
      Put ('-');
      Put (Current_Year);
      Put (", ");
      Put (Copyright_Holder);
      New_Line;
   end Display_Version;
   ----------------------
   -- Ensure_Directory --
   ----------------------

   function Ensure_Directory (Path : String) return String is
   begin
      if Path'Length = 0
        or else Path (Path'Last) = Directory_Separator
        or else Path (Path'Last) = '/' -- on Windows check also for /
      then
         return Path;
      else
         return Path & Directory_Separator;
      end if;
   end Ensure_Directory;

--     ---------------
--     -- Error_Msg --
--     ---------------
--
--     procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr) is
--        pragma Warnings (Off, Msg);
--        pragma Warnings (Off, Flag_Location);
--     begin
--        null;
--     end Error_Msg;
--
--     -----------------
--     -- Error_Msg_S --
--     -----------------
--
--     procedure Error_Msg_S (Msg : String) is
--        pragma Warnings (Off, Msg);
--     begin
--        null;
--     end Error_Msg_S;
--
--     ------------------
--     -- Error_Msg_SC --
--     ------------------
--
--     procedure Error_Msg_SC (Msg : String) is
--        pragma Warnings (Off, Msg);
--     begin
--        null;
--     end Error_Msg_SC;
--
--     ------------------
--     -- Error_Msg_SP --
--     ------------------
--
--     procedure Error_Msg_SP (Msg : String) is
--        pragma Warnings (Off, Msg);
--     begin
--        null;
--     end Error_Msg_SP;

   --------------
   -- File_MD5 --
   --------------

   function File_MD5 (Pathname : String) return Message_Digest is
      use Stream_IO;

      C : Context;
      S : Stream_IO.File_Type;
      B : Stream_Element_Array (1 .. 100 * 1024);
      --  Buffer to read chunk of data
      L : Stream_Element_Offset;
   begin
      Open (S, In_File, Pathname);

      while not End_Of_File (S) loop
         Read (S, B, L);
         Update (C, B (1 .. L));
      end loop;

      Close (S);

      return Digest (C);
   end File_MD5;

   ------------------------------
   -- Get_Compiler_Driver_Path --
   ------------------------------

   function Get_Compiler_Driver_Path
     (Project_Tree : Project_Tree_Ref;
      Lang         : Language_Ptr) return String_Access
   is
      pragma Unreferenced (Project_Tree);
   begin
      if Lang.Config.Compiler_Driver_Path = null then
         declare
            Compiler : Name_Id := Compiler_Subst_HTable.Get (Lang.Name);
         begin
            --  If --compiler-subst was used to specify an alternate compiler,
            --  then Compiler /= No_Name. In the usual case, Compiler =
            --  No_Name, so we set Compiler to the Compiler_Driver from the
            --  config file.

            if Compiler = No_Name then
               Compiler := Name_Id (Lang.Config.Compiler_Driver);
            end if;

            --  No compiler found, return now

            if Compiler = No_Name then
               return null;
            end if;

            declare
               Compiler_Name : constant String := Get_Name_String (Compiler);
            begin
               if Compiler_Name = "" then
                  return null;
               end if;

               Lang.Config.Compiler_Driver_Path :=
                 Locate_Exec_On_Path (Compiler_Name);

               if Lang.Config.Compiler_Driver_Path = null then
                  raise Constraint_Error
                    with "unable to locate """ & Compiler_Name & '"';
               end if;
            end;
         end;
      end if;

      return Lang.Config.Compiler_Driver_Path;
   end Get_Compiler_Driver_Path;

   ----------------------
   -- Get_Slaves_Hosts --
   ----------------------

   function Get_Slaves_Hosts
     (Project_Tree : Project_Tree_Ref;
      Arg          : String) return String
   is
      use Ada.Strings.Unbounded;
      Hosts : Unbounded_String;
   begin
      if Arg'Length > Distributed_Option'Length
        and then Arg (Arg'First + Distributed_Option'Length) = '='
      then
         --  The hosts are specified on the command-line
         Hosts := To_Unbounded_String
           (Arg (Arg'First + Distributed_Option'Length + 1 .. Arg'Last));

      elsif Environment_Variables.Exists ("GPR_SLAVES") then
         Hosts := To_Unbounded_String (Value ("GPR_SLAVES"));

      elsif Environment_Variables.Exists ("GPR_SLAVES_FILE") then
         declare
            F_Name : constant String := Value ("GPR_SLAVES_FILE");
            F      : Text_IO.File_Type;
            Buffer : String (1 .. 100);
            Last   : Natural;
         begin
            if Ada.Directories.Exists (F_Name) then
               Open (F, In_File, F_Name);

               while not Text_IO.End_Of_File (F) loop
                  Text_IO.Get_Line (F, Buffer, Last);

                  if Last > 0 then
                     if Hosts /= Null_Unbounded_String then
                        Append (Hosts, ",");
                     end if;
                     Append (Hosts, Buffer (1 .. Last));
                  end if;
               end loop;

               Text_IO.Close (F);

            else
               Fail_Program
                 (Project_Tree,
                  "hosts distributed file " & F_Name & " not found");
            end if;
         end;
      end if;

      return To_String (Hosts);
   end Get_Slaves_Hosts;

   ----------------------------
   -- Find_Binding_Languages --
   ----------------------------

   procedure Find_Binding_Languages
     (Tree         : Project_Tree_Ref;
      Root_Project : Project_Id)
   is
      Data    : constant Builder_Data_Access := Builder_Data (Tree);
      B_Index : Binding_Data;

      Language_Name      : Name_Id;
      Binder_Driver_Name : File_Name_Type := No_File;
      Binder_Driver_Path : String_Access;
      Binder_Prefix      : Name_Id;
      Language           : Language_Ptr;

      Config  : Language_Config;
      Project : Project_List;

   begin
      --  Have we already processed this tree ?

      if Data.There_Are_Binder_Drivers
        and then Data.Binding /= null
      then
         return;
      end if;

      if Current_Verbosity = High then
         Debug_Output ("Find_Binding_Languages for", Debug_Name (Tree));
      end if;

      Data.There_Are_Binder_Drivers := False;

      Project := Tree.Projects;
      while Project /= null loop
         Language := Project.Project.Languages;

         while Language /= No_Language_Index loop
            Config := Language.Config;

            Binder_Driver_Name := Config.Binder_Driver;

            if Language.First_Source /= No_Source
              and then Binder_Driver_Name /= No_File
            then
               Data.There_Are_Binder_Drivers := True;
               Language_Name := Language.Name;

               B_Index := Data.Binding;
               while B_Index /= null
                 and then B_Index.Language_Name /= Language_Name
               loop
                  B_Index := B_Index.Next;
               end loop;

               if B_Index = null then
                  Get_Name_String (Binder_Driver_Name);
                  Binder_Driver_Path :=
                    Locate_Exec_On_Path (Name_Buffer (1 .. Name_Len));

                  if Binder_Driver_Path = null then
                     Fail_Program
                       (Tree,
                        "unable to find binder driver " &
                        Name_Buffer (1 .. Name_Len));
                  end if;

                  if Current_Verbosity = High then
                     Debug_Output
                       ("Binder_Driver=" & Binder_Driver_Path.all
                        & " for Lang", Language_Name);
                  end if;

                  if Config.Binder_Prefix = No_Name then
                     Binder_Prefix := Empty_String;
                  else
                     Binder_Prefix := Config.Binder_Prefix;
                  end if;

                  B_Index := Data.Binding;
                  while B_Index /= null loop
                     if Binder_Prefix = B_Index.Binder_Prefix then
                        Fail_Program
                          (Tree,
                           "binding prefix cannot be the same for"
                           & " two languages");
                     end if;
                     B_Index := B_Index.Next;
                  end loop;

                  Data.Binding := new Binding_Data_Record'
                    (Language           => Language,
                     Language_Name      => Language_Name,
                     Binder_Driver_Name => Binder_Driver_Name,
                     Binder_Driver_Path => Binder_Driver_Path,
                     Binder_Prefix      => Binder_Prefix,
                     Next               => Data.Binding);
               end if;
            end if;

            Language := Language.Next;
         end loop;

         Project := Project.Next;
      end loop;

      if Root_Project.Qualifier = Aggregate then
         declare
            Agg : Aggregated_Project_List := Root_Project.Aggregated_Projects;
         begin
            while Agg /= null loop
               Find_Binding_Languages (Agg.Tree, Agg.Project);
               Agg := Agg.Next;
            end loop;
         end;
      end if;
   end Find_Binding_Languages;

   ----------------
   -- Get_Target --
   ----------------

   function Get_Target return String is
   begin
      if Target_Name = null or else Target_Name.all = "" then
         return GprConfig.Sdefault.Hostname;
      else
         return Target_Name.all;
      end if;
   end Get_Target;

   --------------------
   -- Locate_Runtime --
   --------------------

   procedure Locate_Runtime
     (Project_Tree : Project_Tree_Ref;
      Language     : Name_Id)
   is
      function Is_RTS_Directory (Path : String) return Boolean;
      --  Returns True if Path is a directory for a runtime. This simply check
      --  that Path has a "adalib" subdirectoy, which is a property for
      --  runtimes on the project path.

      function Is_Base_Name (Path : String) return Boolean;
      --  Returns True if Path has no directory separator

      ----------------------
      -- Is_RTS_Directory --
      ----------------------

      function Is_RTS_Directory (Path : String) return Boolean is
      begin
         return Is_Directory (Path & Directory_Separator & "adalib");
      end Is_RTS_Directory;

      --  Local declarations

      function Find_Rts_In_Path is new GPR.Env.Find_Name_In_Path
        (Check_Filename => Is_RTS_Directory);

      ------------------
      -- Is_Base_Name --
      ------------------

      function Is_Base_Name (Path : String) return Boolean is
      begin
         for I in Path'Range loop
            if Path (I) = Directory_Separator or else Path (I) = '/' then
               return False;
            end if;
         end loop;
         return True;
      end Is_Base_Name;

      RTS_Name : constant String := GPR.Conf.Runtime_Name_For (Language);

      Full_Path : String_Access;

   begin
      Full_Path := Find_Rts_In_Path (Root_Environment.Project_Path, RTS_Name);
      if Full_Path /= null then
         GPR.Conf.Set_Runtime_For
           (Language, Normalize_Pathname (Full_Path.all));
         Free (Full_Path);
      elsif not Is_Base_Name (RTS_Name) then
         Fail_Program (Project_Tree, "cannot find RTS " & RTS_Name);
      end if;
   end Locate_Runtime;

   ------------------------------
   -- Look_For_Default_Project --
   ------------------------------

   procedure Look_For_Default_Project (Never_Fail : Boolean := False) is
   begin
      No_Project_File_Found := False;

      if Is_Regular_File (Default_Project_File_Name) then
         Project_File_Name := new String'(Default_Project_File_Name);

      else
         --  Check if there is a single project file in the current
         --  directory. If there is one and only one, use it.

         declare
            Dir : Dir_Type;
            Str : String (1 .. 255);
            Last : Natural;
            Single : String_Access := null;

         begin
            No_Project_File_Found := True;

            Open (Dir, ".");

            loop
               Read (Dir, Str, Last);
               exit when Last = 0;

               if Last > Project_File_Extension'Length
                 and then Is_Regular_File (Str (1 .. Last))
               then
                  Canonical_Case_File_Name (Str (1 .. Last));

                  if Str (Last - Project_File_Extension'Length + 1 .. Last)
                    = Project_File_Extension
                  then
                     No_Project_File_Found := False;

                     if Single = null then
                        Single := new String'(Str (1 .. Last));

                     else
                        --  There are several project files in the current
                        --  directory. Reset Single to null and exit.

                        Single := null;
                        exit;
                     end if;
                  end if;
               end if;
            end loop;

            Close (Dir);

            Project_File_Name := Single;
         end;

         if No_Project_File_Found or else
            (Never_Fail and then Project_File_Name = null)
         then
            Project_File_Name :=
              new String'(Executable_Prefix_Path & Implicit_Project_File_Path);

            if not Is_Regular_File (Project_File_Name.all) then
               Project_File_Name := null;
            end if;
         end if;
      end if;

      if (not Quiet_Output) and then Project_File_Name /= null then
         Put ("using project file ");
         Put_Line (Project_File_Name.all);
      end if;
   end Look_For_Default_Project;

   -------------------
   -- Major_Id_Name --
   -------------------

   function Major_Id_Name
     (Lib_Filename : String;
      Lib_Version  : String)
      return String
   is
      Maj_Version : constant String := Lib_Version;
      Last_Maj    : Positive;
      Last        : Positive;
      Ok_Maj      : Boolean := False;

   begin
      Last_Maj := Maj_Version'Last;
      while Last_Maj > Maj_Version'First loop
         if Maj_Version (Last_Maj) in '0' .. '9' then
            Last_Maj := Last_Maj - 1;

         else
            Ok_Maj := Last_Maj /= Maj_Version'Last and then
            Maj_Version (Last_Maj) = '.';

            if Ok_Maj then
               Last_Maj := Last_Maj - 1;
            end if;

            exit;
         end if;
      end loop;

      if Ok_Maj then
         Last := Last_Maj;
         while Last > Maj_Version'First loop
            if Maj_Version (Last) in '0' .. '9' then
               Last := Last - 1;

            else
               Ok_Maj := Last /= Last_Maj and then
               Maj_Version (Last) = '.';

               if Ok_Maj then
                  Last := Last - 1;
                  Ok_Maj :=
                    Maj_Version (Maj_Version'First .. Last) = Lib_Filename;
               end if;

               exit;
            end if;
         end loop;
      end if;

      if Ok_Maj then
         return Maj_Version (Maj_Version'First .. Last_Maj);
      else
         return "";
      end if;
   end Major_Id_Name;

   ------------------
   -- Partial_Name --
   ------------------

   function Partial_Name
     (Lib_Name      : String;
      Number        : Natural;
      Object_Suffix : String) return String
   is
      Img : constant String := Number'Img;
   begin
      return
        Partial_Prefix & Lib_Name &
        '_' & Img (Img'First + 1 .. Img'Last)
        & Object_Suffix;
   end Partial_Name;

   --------------------------------
   -- Project_Compilation_Failed --
   --------------------------------

   function Project_Compilation_Failed
     (Prj       : Project_Id;
      Recursive : Boolean := True) return Boolean
   is
      use Project_Name_Boolean_Htable;
   begin
      if Get (Project_Failure, Prj.Name) then
         return True;

      elsif not Recursive then
         return False;

      else
         --  Check all imported projects directly or indirectly
         declare
            Plist : Project_List := Prj.All_Imported_Projects;
         begin
            while Plist /= null loop
               if Get (Project_Failure, Plist.Project.Name) then
                  return True;
               else
                  Plist := Plist.Next;
               end if;
            end loop;
            return False;
         end;
      end if;
   end Project_Compilation_Failed;

   -----------------------------------
   -- Set_Failed_Compilation_Status --
   -----------------------------------

   procedure Set_Failed_Compilation_Status (Prj : Project_Id) is
   begin
      Project_Name_Boolean_Htable.Set (Project_Failure, Prj.Name, True);
   end Set_Failed_Compilation_Status;

   -----------------------
   -- Shared_Libgcc_Dir --
   -----------------------

   function Shared_Libgcc_Dir (Run_Time_Dir : String) return String is
      Path      : String (1 .. Run_Time_Dir'Length + 15);
      Path_Last : constant Natural := Run_Time_Dir'Length;
      GCC_Index : Natural := 0;

   begin
      Path (1 .. Path_Last) := Run_Time_Dir;
      GCC_Index := Index (Path (1 .. Path_Last), "gcc-lib");

      if GCC_Index /= 0 then
         --  This is gcc 2.8.2: the shared version of libgcc is
         --  located in the parent directory of "gcc-lib".

         GCC_Index := GCC_Index - 1;

      else
         GCC_Index := Index (Path (1 .. Path_Last), "/lib/");

         if GCC_Index = 0 then
            GCC_Index :=
              Index
                (Path (1 .. Path_Last),
                 Directory_Separator & "lib" & Directory_Separator);
         end if;

         if GCC_Index /= 0 then
            --  We have found "lib" as a subdirectory in the runtime dir path.
            --  The
            declare
               Subdir : constant String :=
                 Interfaces.C.Strings.Value (Libgcc_Subdir_Ptr);
            begin
               Path
                 (GCC_Index + 1 ..
                    GCC_Index + Subdir'Length) :=
                   Subdir;
               GCC_Index :=
                 GCC_Index + Subdir'Length;
            end;
         end if;
      end if;

      return Path (1 .. GCC_Index);
   end Shared_Libgcc_Dir;

   ---------------------
   -- Need_To_Compile --
   ---------------------

   procedure Need_To_Compile
     (Source         : GPR.Source_Id;
      Tree           : Project_Tree_Ref;
      In_Project     : Project_Id;
      Must_Compile   : out Boolean;
      The_ALI        : out ALI.ALI_Id;
      Object_Check   : Boolean;
      Always_Compile : Boolean)
   is
      Source_Path        : constant String :=
                             Get_Name_String (Source.Path.Display_Name);
      C_Source_Path      : constant String :=
                             Get_Name_String (Source.Path.Name);
      Runtime_Source_Dirs : constant Name_List_Index :=
                             Source.Language.Config.Runtime_Source_Dirs;

      Start    : Natural;
      Finish   : Natural;
      Last_Obj : Natural;
      Stamp    : Time_Stamp_Type;

      Looping : Boolean := False;
      --  Set to True at the end of the first Big_Loop for Makefile fragments

      Source_In_Dependencies : Boolean := False;
      --  Set True if source was found in dependency file of its object file

      C_Object_Name : String_Access := null;
      --  The canonical file name for the object file

      Object_Path   : String_Access := null;
      --  The absolute path name for the object file

      Switches_Name : String_Access := null;
      --  The file name of the file that contains the switches that were used
      --  in the last compilation.

      Num_Ext : Natural;
      --  Number of extending projects

      ALI_Project : Project_Id;
      --  If the ALI file is in the object directory of a project, this is
      --  the project id.

      Externally_Built : constant Boolean := In_Project.Externally_Built;
      --  True if the project of the source is externally built

      function Process_Makefile_Deps
        (Dep_Name, Obj_Dir : String)    return Boolean;
      function Process_ALI_Deps         return Boolean;
      function Process_ALI_Closure_Deps return Boolean;
      --  Process the dependencies for the current source file for the various
      --  dependency modes.
      --  They return True if the file needs to be recompiled

      procedure Cleanup;
      --  Cleanup local variables

      function Check_Time_Stamps
        (Path  : String;
         Stamp : Time_Stamp_Type)
         return Boolean;

      -----------------------
      -- Check_Time_Stamps --
      -----------------------

      function Check_Time_Stamps
        (Path  : String;
         Stamp : Time_Stamp_Type)
         return Boolean
      is
      begin
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Path);

         declare
            TS   : constant Time_Stamp_Type :=
              File_Stamp (Path_Name_Type'(Name_Find));
         begin
            if TS /= Empty_Time_Stamp and then TS /= Stamp then
               if Opt.Verbosity_Level > Opt.Low then
                  Put_Line ("   -> different time stamp for " & Path);

                  if Debug.Debug_Flag_T then
                     Put_Line ("   in ALI file: " & String (Stamp));
                     Put_Line ("   actual file: " & String (TS));
                  end if;
               end if;

               return True;
            end if;
         end;

         return False;
      end Check_Time_Stamps;

      ---------------------------
      -- Process_Makefile_Deps --
      ---------------------------

      function Process_Makefile_Deps
        (Dep_Name, Obj_Dir : String) return Boolean
      is
         Dep_File : GPR.Util.Text_File;
         Last_Source : String_Access;
         Last_TS     : Time_Stamp_Type := Empty_Time_Stamp;

         function Is_Time_Stamp (S : String) return Boolean;
         --  Return True iff S has the format of a Time_Stamp_Type

         -------------------
         -- Is_Time_Stamp --
         -------------------

         function Is_Time_Stamp (S : String) return Boolean is
            Result : Boolean := False;
         begin
            if S'Length = Time_Stamp_Length then
               Result := True;

               for J in S'Range loop
                  if S (J) not in '0' .. '9' then
                     Result := False;
                     exit;
                  end if;
               end loop;
            end if;

            return Result;
         end Is_Time_Stamp;

      begin
         Open (Dep_File, Dep_Name);

         --  If dependency file cannot be open, we need to recompile
         --  the source.

         if not Is_Valid (Dep_File) then
            if Opt.Verbosity_Level > Opt.Low then
               Put  ("      -> could not open dependency file ");
               Put_Line (Dep_Name);
            end if;

            return True;
         end if;

         --  Loop Big_Loop is executed several times only when the
         --  dependency file contains several times
         --     <object file>: <source1> ...
         --  When there is only one of such occurence, Big_Loop is exited
         --  successfully at the beginning of the second loop.

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
                     --  Skip a first line that is an empty continuation line

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

               --  If dependency file contains only empty lines or comments,
               --  then dependencies are unknown, and the source needs to be
               --  recompiled.

               if End_Of_File_Reached then
                  --  If we have reached the end of file after the first
                  --  loop, there is nothing else to do.

                  exit Big_Loop when Looping;

                  if Opt.Verbosity_Level > Opt.Low then
                     Put  ("      -> dependency file ");
                     Put  (Dep_Name);
                     Put_Line (" is empty");
                  end if;

                  Close (Dep_File);
                  return True;
               end if;
            end;

            Start  := 1;
            Finish := Index (Name_Buffer (1 .. Name_Len), ": ");

            if Finish = 0 then
               Finish :=
                 Index
                   (Name_Buffer (1 .. Name_Len), (1 => ':', 2 => ASCII.HT));
            end if;

            if Finish /= 0 then
               Last_Obj := Finish;
               loop
                  Last_Obj := Last_Obj - 1;
                  exit when Last_Obj = Start
                    or else Name_Buffer (Last_Obj) /= ' ';
               end loop;

               while Start < Last_Obj and then Name_Buffer (Start) = ' ' loop
                  Start := Start + 1;
               end loop;

               Canonical_Case_File_Name (Name_Buffer (Start .. Last_Obj));
            end if;

            --  First line must start with name of object file, followed by
            --  colon.

            if Finish = 0
              or else
                (C_Object_Name /= null
                 and then Name_Buffer (Start .. Last_Obj) /= C_Object_Name.all)
            then
               if Opt.Verbosity_Level > Opt.Low then
                  Put  ("      -> dependency file ");
                  Put  (Dep_Name);
                  Put_Line (" has wrong format");

                  if Finish = 0 then
                     Put_Line ("         no colon");

                  else
                     Put  ("         expected object file name ");
                     Put  (C_Object_Name.all);
                     Put  (", got ");
                     Put_Line (Name_Buffer (Start .. Last_Obj));
                  end if;
               end if;

               Close (Dep_File);
               return True;

            else
               Start := Finish + 2;

               --  Process each line

               Line_Loop : loop
                  declare
                     Line : String  := Name_Buffer (1 .. Name_Len);
                     Last : Natural := Name_Len;

                  begin
                     Name_Loop : loop

                        --  Find the beginning of the next source path name

                        while Start <= Last and then Line (Start) = ' ' loop
                           Start := Start + 1;
                        end loop;

                        exit Line_Loop when Start > Last;

                        --  Go to next line when there is a continuation
                        --  character \ at the end of the line.

                        exit Name_Loop when Start = Last
                          and then Line (Start) = '\';

                        --  We should not be at the end of the line, without
                        --  a continuation character \.

                        if Start = Last then
                           if Opt.Verbosity_Level > Opt.Low then
                              Put  ("      -> dependency file ");
                              Put  (Dep_Name);
                              Put_Line (" has wrong format");
                           end if;

                           Close (Dep_File);
                           return True;
                        end if;

                        --  Look for the end of the source path name

                        Finish := Start;

                        while Finish < Last loop
                           if Line (Finish) = '\' then
                              --  On Windows, a '\' is part of the path
                              --  name, except when it is not the first
                              --  character followed by another '\' or by a
                              --  space. On other platforms, when we are
                              --  getting a '\' that is not the last
                              --  character of the line, the next character
                              --  is part of the path name, even if it is a
                              --  space.

                              if On_Windows
                                and then Finish = Start
                                and then Line (Finish + 1) = '\'
                              then
                                 Finish := Finish + 2;

                              elsif On_Windows
                                and then Line (Finish + 1) /= '\'
                                and then Line (Finish + 1) /= ' '
                              then
                                 Finish := Finish + 1;

                              else
                                 Line (Finish .. Last - 1) :=
                                   Line (Finish + 1 .. Last);
                                 Last := Last - 1;
                              end if;

                           else
                              --  A space that is not preceded by '\'
                              --  indicates the end of the path name.

                              exit when Line (Finish + 1) = ' ';
                              Finish := Finish + 1;
                           end if;
                        end loop;

                        if Last_Source /= null
                          and then Is_Time_Stamp (Line (Start .. Finish))
                        then
                           --  If we have a time stamp, check if it is the
                           --  same as the source time stamp.

                           declare
                              Tstring : constant
                                String (1 .. Time_Stamp_Length) :=
                                Line (Start .. Finish);
                              TS : constant Time_Stamp_Type :=
                                Time_Stamp_Type (Tstring);
                              OK : constant Boolean := Last_TS = TS;

                           begin
                              if not OK and then Opt.Verbosity_Level > Opt.Low
                              then
                                 Put ("      -> source ");
                                 Put  (Last_Source.all);
                                 Put_Line
                                   (" has modified time stamp");
                              end if;

                              Free (Last_Source);

                              if not OK then
                                 Close (Dep_File);
                                 return True;
                              end if;
                           end;

                        else
                           --  Check this source

                           declare
                              Src_Name : constant String :=
                                Normalize_Pathname
                                  (Name           => Line (Start .. Finish),
                                   Directory      => Obj_Dir,
                                   Resolve_Links  => False);
                              C_Src_Name : String := Src_Name;
                              Src_TS   : Time_Stamp_Type;
                              Source_2 : GPR.Source_Id;

                           begin
                              Canonical_Case_File_Name (C_Src_Name);

                              --  If it is original source, set
                              --  Source_In_Dependencies.

                              if C_Src_Name = C_Source_Path then
                                 Source_In_Dependencies := True;
                              end if;

                              --  Get the time stamp of the source, which is
                              --  not necessarily a source of any project.

                              Name_Len := 0;
                              Add_Str_To_Name_Buffer (Src_Name);
                              Src_TS := File_Stamp
                                           (Path_Name_Type'(Name_Find));

                              --  If the source does not exist, we need to
                              --  recompile.

                              if Src_TS = Empty_Time_Stamp then
                                 if Opt.Verbosity_Level > Opt.Low then
                                    Put  ("      -> source ");
                                    Put  (Src_Name);
                                    Put_Line (" does not exist");
                                 end if;

                                 Close (Dep_File);
                                 return True;

                              --  If the source has been modified after the
                              --  object file, we need to recompile.

                              elsif Object_Check
                                and then
                                  Source.Language.Config.Object_Generated
                                and then Src_TS > Source.Object_TS
                              then
                                 if Opt.Verbosity_Level > Opt.Low then
                                    Put ("      -> source ");
                                    Put (Src_Name);
                                    Put_Line (" more recent than object file");
                                 end if;

                                 Close (Dep_File);
                                 return True;

                              else
                                 Name_Len := Src_Name'Length;
                                 Name_Buffer (1 .. Name_Len) := Src_Name;
                                 Source_2 := Source_Paths_Htable.Get
                                   (Tree.Source_Paths_HT, Name_Find);

                                 if Source_2 /= No_Source
                                   and then Source_2.Replaced_By /= No_Source
                                 then
                                    if Opt.Verbosity_Level > Opt.Low then
                                       Put  ("      -> source ");
                                       Put  (Src_Name);
                                       Put_Line (" has been replaced");
                                    end if;

                                    Close (Dep_File);
                                    return True;

                                 else
                                    Last_Source := new String'(Src_Name);
                                    Last_TS     := Src_TS;
                                 end if;
                              end if;
                           end;
                        end if;

                        --  If the source path name ends the line, we are
                        --  done.

                        exit Line_Loop when Finish = Last;

                        --  Go get the next source on the line

                        Start := Finish + 1;
                     end loop Name_Loop;
                  end;

                  --  If we are here, we had a continuation character \ at
                  --  the end of the line, so we continue with the next
                  --  line.

                  Get_Line (Dep_File, Name_Buffer, Name_Len);
                  Start  := 1;
                  Finish := 1;
               end loop Line_Loop;
            end if;

            --  Set Looping at the end of the first loop
            Looping := True;
         end loop Big_Loop;

         Close (Dep_File);

         --  If the original sources were not in the dependency file, then
         --  we need to recompile. It may mean that we are using a different
         --  source (different variant) for this object file.

         if not Source_In_Dependencies then
            if Opt.Verbosity_Level > Opt.Low then
               Put  ("      -> source ");
               Put  (Source_Path);
               Put_Line (" is not in the dependencies");
            end if;

            return True;
         end if;

         return False;
      end Process_Makefile_Deps;

      ----------------------
      -- Process_ALI_Deps --
      ----------------------

      function Process_ALI_Deps return Boolean is
         Text     : Text_Buffer_Ptr :=
                      Read_Library_Info_From_Full
                       (File_Name_Type (Source.Dep_Path),
                        Source.Dep_TS'Access);
         Sfile    : File_Name_Type;
         Dep_Src  : GPR.Source_Id;
         Proj     : Project_Id;

         Found : Boolean := False;

      begin
         if Text = null then
            if Opt.Verbosity_Level > Opt.Low then
               Put ("    -> cannot read ");
               Put_Line (Get_Name_String (Source.Dep_Path));
            end if;

            return True;
         end if;

         --  Read only the necessary lines of the ALI file

         The_ALI :=
           ALI.Scan_ALI
             (File_Name_Type (Source.Dep_Path),
              Text,
              Ignore_ED     => False,
              Err           => True,
              Read_Lines    => "PDW");
         Free (Text);

         if The_ALI = ALI.No_ALI_Id then
            if Opt.Verbosity_Level > Opt.Low then
               Put ("    -> ");
               Put (Get_Name_String (Source.Dep_Path));
               Put_Line (" is incorrectly formatted");
            end if;

            return True;
         end if;

         if ALI.ALIs.Table (The_ALI).Compile_Errors then
            if Opt.Verbosity_Level > Opt.Low then
               Put_Line ("    -> last compilation had errors");
            end if;

            return True;
         end if;

         if Object_Check and then ALI.ALIs.Table (The_ALI).No_Object then
            if Opt.Verbosity_Level > Opt.Low then
               Put_Line
                 ("    -> no object generated during last compilation");
            end if;

            return True;
         end if;

         if Check_Source_Info_In_ALI (The_ALI, Tree) = No_Name then
            return True;
         end if;

         --  We need to check that the ALI file is in the correct object
         --  directory. If it is in the object directory of a project
         --  that is extended and it depends on a source that is in one
         --  of its extending projects, then the ALI file is not in the
         --  correct object directory.

         ALI_Project := Source.Object_Project;

         --  Count the extending projects

         Num_Ext := 0;
         Proj := ALI_Project;
         loop
            Proj := Proj.Extended_By;
            exit when Proj = No_Project;
            Num_Ext := Num_Ext + 1;
         end loop;

         declare
            Projects : array (1 .. Num_Ext) of Project_Id;
         begin
            Proj := ALI_Project;
            for J in Projects'Range loop
               Proj := Proj.Extended_By;
               Projects (J) := Proj;
            end loop;

            for D in ALI.ALIs.Table (The_ALI).First_Sdep ..
              ALI.ALIs.Table (The_ALI).Last_Sdep
            loop
               Sfile := ALI.Sdep.Table (D).Sfile;

               if ALI.Sdep.Table (D).Stamp /= Empty_Time_Stamp then
                  Dep_Src := Source_Files_Htable.Get
                    (Tree.Source_Files_HT, Sfile);
                  Found := False;

                  if Dep_Src = No_Source and then
                    ALI.Sdep.Table (D).Checksum = 0
                  then
                     --  Probably preprocessing dependencies. Look for the
                     --  file in the object directory.

                     declare
                        Path : Path_Name_Type;
                        File : constant String := Get_Name_String (Sfile);
                        Stamp : Time_Stamp_Type;

                     begin
                        if Is_Absolute_Path (File) then
                           Path := Path_Name_Type (Sfile);
                        else
                           Name_Len := 0;
                           Add_Str_To_Name_Buffer (Source_Dir_Of (Source));
                           Add_Char_To_Name_Buffer (Directory_Separator);
                           Add_Str_To_Name_Buffer (File);
                           Path := Name_Find;
                        end if;

                        Stamp := File_Stamp (Path);

                        if Stamp /= ALI.Sdep.Table (D).Stamp then
                           if Opt.Verbosity_Level > Opt.Low then
                              if Stamp = Empty_Time_Stamp then
                                 Put ("  -> """);
                                 Put (Get_Name_String (Sfile));
                                 Put_Line (""" missing");

                              else
                                 Put ("   -> different time stamp for ");
                                 Put_Line (Get_Name_String (Path));

                                 if Debug.Debug_Flag_T then
                                    Put ("   in ALI file: ");
                                    Put_Line
                                      (String (ALI.Sdep.Table (D).Stamp));
                                    Put ("   actual file: ");
                                    Put_Line (String (Stamp));
                                 end if;
                              end if;
                           end if;

                           return True;
                        end if;
                     end;

                  else
                     if Dep_Src = No_Source and then
                       not Is_Ada_Predefined_File_Name (Sfile)
                     then
                        if Opt.Verbosity_Level > Opt.Low then
                           Put ("  -> """);
                           Put (Get_Name_String (Sfile));
                           Put_Line (""" missing");
                        end if;

                        return True;
                     end if;

                     while Dep_Src /= No_Source loop
                        Initialize_Source_Record (Dep_Src);

                        if not Dep_Src.Locally_Removed
                          and then Dep_Src.Unit /= No_Unit_Index
                        then
                           Found := True;

                           if Opt.Minimal_Recompilation
                             and then ALI.Sdep.Table (D).Stamp /=
                             Dep_Src.Source_TS
                           then
                              --  If minimal recompilation is in action,
                              --  replace the stamp of the source file in
                              --  the table if checksums match.

                              declare
                                 Source_Index : Source_File_Index;
                                 use Scans;

                              begin
                                 Source_Index :=
                                   Sinput.Load_File
                                     (Get_Name_String
                                        (Dep_Src.Path.Display_Name));

                                 if Source_Index /= No_Source_File then

                                    Err.Scanner.Initialize_Scanner
                                      (Source_Index, Err.Scanner.Ada);

                                    --  Scan the complete file to compute its
                                    --  checksum.

                                    loop
                                       Err.Scanner.Scan;
                                       exit when Token = Tok_EOF;
                                    end loop;

                                    if Scans.Checksum =
                                      ALI.Sdep.Table (D).Checksum
                                    then
                                       if Opt.Verbosity_Level > Opt.Low then
                                          Put ("   ");
                                          Put
                                            (Get_Name_String
                                               (ALI.Sdep.Table (D).Sfile));
                                          Put (": up to date, " &
                                                 "different timestamps " &
                                                 "but same checksum");
                                          New_Line;
                                       end if;

                                       ALI.Sdep.Table (D).Stamp :=
                                         Dep_Src.Source_TS;
                                    end if;
                                 end if;

                                 --  To avoid using too much memory, free the
                                 --  memory allocated.

                                 Sinput.Clear_Source_File_Table;
                              end;
                           end if;

                           if ALI.Sdep.Table (D).Stamp /= Dep_Src.Source_TS
                           then
                              if Opt.Verbosity_Level > Opt.Low then
                                 Put
                                   ("   -> different time stamp for ");
                                 Put_Line (Get_Name_String (Sfile));

                                 if Debug.Debug_Flag_T then
                                    Put ("   in ALI file: ");
                                    Put_Line
                                      (String (ALI.Sdep.Table (D).Stamp));
                                    Put ("   actual file: ");
                                    Put_Line (String (Dep_Src.Source_TS));
                                 end if;
                              end if;

                              return True;

                           else
                              for J in Projects'Range loop
                                 if Dep_Src.Project = Projects (J) then
                                    if Opt.Verbosity_Level > Opt.Low then
                                       Put_Line
                                         ("   -> wrong object directory");
                                    end if;

                                    return True;
                                 end if;
                              end loop;

                              exit;
                           end if;
                        end if;

                        Dep_Src := Dep_Src.Next_With_File_Name;
                     end loop;

                     --  If the source was not found and the runtime source
                     --  directory is defined, check if the file exists there,
                     --  and if it does, check its timestamp.

                     if not Found
                       and then
                         (Runtime_Source_Dirs /= No_Name_List
                          or else
                          Is_Absolute_Path (Get_Name_String (Sfile)))
                     then
                        if Is_Absolute_Path (Get_Name_String (Sfile)) then
                           if Check_Time_Stamps
                             (Get_Name_String (Sfile),
                              ALI.Sdep.Table (D).Stamp)
                           then
                              return True;
                           end if;

                        else
                           declare
                              R_Dirs : Name_List_Index := Runtime_Source_Dirs;
                           begin
                              while R_Dirs /= No_Name_List loop
                                 declare
                                    Nam_Nod : constant Name_Node :=
                                      Tree.Shared.Name_Lists.Table (R_Dirs);
                                 begin
                                    if Check_Time_Stamps
                                      (Get_Name_String (Nam_Nod.Name) &
                                         Directory_Separator &
                                         Get_Name_String (Sfile),
                                       ALI.Sdep.Table (D).Stamp)
                                    then
                                       return True;
                                    end if;

                                    R_Dirs := Nam_Nod.Next;
                                 end;
                              end loop;
                           end;
                        end if;
                     end if;
                  end if;
               end if;
            end loop;
         end;

         return False;
      end Process_ALI_Deps;

      package Processed_Sources is new GNAT.Table
        (Table_Component_Type => GPR.Source_Id,
         Table_Index_Type     => Positive,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100);

      ------------------------------
      -- Process_ALI_Closure_Deps --
      ------------------------------

      function Process_ALI_Closure_Deps return Boolean is
         Attr : aliased File_Attributes := Unknown_Attributes;
         Text     : Text_Buffer_Ptr :=
                      Read_Library_Info_From_Full
                        (File_Name_Type (Source.Dep_Path), Attr'Access);
         Sfile    : File_Name_Type;
         Dep_Src  : GPR.Source_Id;
         Proj     : Project_Id;
         TS0      : Time_Stamp_Type;

         Found : Boolean := False;

         Last_Processed_Source : Natural := 0;
         Next_Source : GPR.Source_Id;
         Insert_Source : Boolean := False;

         Other_ALI : ALI.ALI_Id;
      begin
         if Text = null then
            if Opt.Verbosity_Level > Opt.Low then
               Put ("    -> cannot read ");
               Put_Line (Get_Name_String (Source.Dep_Path));
            end if;

            return True;
         end if;

         TS0 := File_Stamp (Source.Dep_Path);

         --  Read only the necessary lines of the ALI file

         The_ALI :=
           ALI.Scan_ALI
             (File_Name_Type (Source.Dep_Path),
              Text,
              Ignore_ED     => False,
              Err           => True,
              Read_Lines    => "PDW");
         Free (Text);

         if The_ALI = ALI.No_ALI_Id then
            if Opt.Verbosity_Level > Opt.Low then
               Put ("    -> ");
               Put (Get_Name_String (Source.Dep_Path));
               Put_Line (" is incorrectly formatted");
            end if;

            return True;
         end if;

         if ALI.ALIs.Table (The_ALI).Compile_Errors then
            if Opt.Verbosity_Level > Opt.Low then
               Put_Line ("    -> last compilation had errors");
            end if;

            return True;
         end if;

         if Object_Check and then ALI.ALIs.Table (The_ALI).No_Object then
            if Opt.Verbosity_Level > Opt.Low then
               Put_Line
                 ("    -> no object generated during last compilation");
            end if;

            return True;
         end if;

         if Check_Source_Info_In_ALI (The_ALI, Tree) = No_Name then
            return True;
         end if;

         Processed_Sources.Init;
         Processed_Sources.Append (Source);
         Last_Processed_Source := 2;

         --  We need to check that the ALI file is in the correct object
         --  directory. If it is in the object directory of a project
         --  that is extended and it depends on a source that is in one
         --  of its extending projects, then the ALI file is not in the
         --  correct object directory.

         ALI_Project := Source.Object_Project;

         --  Count the extending projects

         Num_Ext := 0;
         Proj := ALI_Project;
         loop
            Proj := Proj.Extended_By;
            exit when Proj = No_Project;
            Num_Ext := Num_Ext + 1;
         end loop;

         declare
            Projects : array (1 .. Num_Ext) of Project_Id;
         begin
            Proj := ALI_Project;
            for J in Projects'Range loop
               Proj := Proj.Extended_By;
               Projects (J) := Proj;
            end loop;

            for D in ALI.ALIs.Table (The_ALI).First_Sdep ..
              ALI.ALIs.Table (The_ALI).Last_Sdep
            loop
               Sfile := ALI.Sdep.Table (D).Sfile;

               if ALI.Sdep.Table (D).Stamp /= Empty_Time_Stamp then
                  Dep_Src := Source_Files_Htable.Get
                    (Tree.Source_Files_HT, Sfile);
                  Found := False;

                  if Dep_Src /= No_Source then
                     Insert_Source := True;
                     for J in 1 .. Processed_Sources.Last loop
                        if Processed_Sources.Table (J) = Dep_Src then
                           Insert_Source := False;
                           exit;
                        end if;
                     end loop;

                     if Insert_Source then
                        Processed_Sources.Append (Dep_Src);
                     end if;
                  end if;

                  while Dep_Src /= No_Source loop
                     Initialize_Source_Record (Dep_Src);

                     if not Dep_Src.Locally_Removed
                       and then Dep_Src.Unit /= No_Unit_Index
                     then
                        Found := True;

                        if Opt.Minimal_Recompilation
                          and then ALI.Sdep.Table (D).Stamp /=
                          Dep_Src.Source_TS
                        then
                           --  If minimal recompilation is in action, replace
                           --  the stamp of the source file in the table if
                           --  checksums match.

                           declare
                              Source_Index : Source_File_Index;
                              use Scans;

                           begin
                              Source_Index :=
                                Sinput.Load_File
                                  (Get_Name_String
                                      (Dep_Src.Path.Display_Name));

                              if Source_Index /= No_Source_File then

                                 Err.Scanner.Initialize_Scanner
                                   (Source_Index, Err.Scanner.Ada);

                                 --  Scan the complete file to compute its
                                 --  checksum.

                                 loop
                                    Err.Scanner.Scan;
                                    exit when Token = Tok_EOF;
                                 end loop;

                                 if Scans.Checksum =
                                   ALI.Sdep.Table (D).Checksum
                                 then
                                    if Opt.Verbosity_Level > Opt.Low then
                                       Put ("   ");
                                       Put
                                         (Get_Name_String
                                            (ALI.Sdep.Table (D).Sfile));
                                       Put (": up to date, " &
                                            "different timestamps " &
                                            "but same checksum");
                                       New_Line;
                                    end if;

                                    ALI.Sdep.Table (D).Stamp :=
                                      Dep_Src.Source_TS;
                                 end if;
                              end if;

                              --  To avoid using too much memory, free the
                              --  memory allocated.

                              Sinput.Clear_Source_File_Table;
                           end;
                        end if;

                        if ALI.Sdep.Table (D).Stamp /= Dep_Src.Source_TS then
                           if Opt.Verbosity_Level > Opt.Low then
                              Put ("   -> different time stamp for ");
                              Put_Line (Get_Name_String (Sfile));

                              if Debug.Debug_Flag_T then
                                 Put ("   in ALI file: ");
                                 Put_Line
                                   (String (ALI.Sdep.Table (D).Stamp));
                                 Put ("   actual file: ");
                                 Put_Line (String (Dep_Src.Source_TS));
                              end if;
                           end if;

                           return True;

                        else
                           for J in Projects'Range loop
                              if Dep_Src.Project = Projects (J) then
                                 if Opt.Verbosity_Level > Opt.Low then
                                    Put_Line
                                      ("   -> wrong object directory");
                                 end if;

                                 return True;
                              end if;
                           end loop;

                           exit;
                        end if;
                     end if;

                     Dep_Src := Dep_Src.Next_With_File_Name;
                  end loop;

                  --  If the source was not found and the runtime source
                  --  directory is defined, check if the file exists there, and
                  --  if it does, check its timestamp.

                  if not Found and then Runtime_Source_Dirs /= No_Name_List
                  then
                     declare
                        R_Dirs : Name_List_Index := Runtime_Source_Dirs;
                     begin
                        while R_Dirs /= No_Name_List loop
                           declare
                              Nam_Nod : constant Name_Node :=
                                Tree.Shared.Name_Lists.Table (R_Dirs);
                           begin
                              if Check_Time_Stamps
                                (Get_Name_String (Nam_Nod.Name) &
                                   Directory_Separator &
                                   Get_Name_String (Sfile),
                                 ALI.Sdep.Table (D).Stamp)
                              then
                                 return True;
                              end if;

                              R_Dirs := Nam_Nod.Next;
                           end;
                        end loop;
                     end;
                  end if;
               end if;
            end loop;
         end;

         while Last_Processed_Source <= Processed_Sources.Last loop
            Next_Source := Processed_Sources.Table (Last_Processed_Source);

            if not Next_Source.Project.Externally_Built
              and then
               (Next_Source.Unit = No_Unit_Index
                or else Next_Source.Kind /= Sep)
            then
               declare
                  Attrib : aliased File_Attributes := Unknown_Attributes;
               begin
                  Text :=
                    Read_Library_Info_From_Full
                      (File_Name_Type (Next_Source.Dep_Path), Attrib'Access);
               end;

               if Text = null then
                  if Opt.Verbosity_Level > Opt.Low then
                     Put ("    -> cannot read ");
                     Put_Line (Get_Name_String (Next_Source.Dep_Path));
                  end if;

                  return True;
               end if;

               --  Read only the necessary lines of the ALI file

               Other_ALI :=
                 ALI.Scan_ALI
                   (File_Name_Type (Next_Source.Dep_Path),
                    Text,
                    Ignore_ED     => False,
                    Err           => True,
                    Read_Lines    => "PDW");
               Free (Text);

               if Other_ALI = ALI.No_ALI_Id then
                  if Opt.Verbosity_Level > Opt.Low then
                     Put ("    -> ");
                     Put (Get_Name_String (Next_Source.Dep_Path));
                     Put_Line (" is incorrectly formatted");
                  end if;

                  return True;
               end if;

               if ALI.ALIs.Table (Other_ALI).Compile_Errors then
                  if Opt.Verbosity_Level > Opt.Low then
                     Put  ("    -> last compilation of ");
                     Put  (Get_Name_String (Next_Source.Dep_Path));
                     Put_Line (" had errors");
                  end if;

                  return True;
               end if;

               for D in ALI.ALIs.Table (Other_ALI).First_Sdep ..
                 ALI.ALIs.Table (Other_ALI).Last_Sdep
               loop
                  Sfile := ALI.Sdep.Table (D).Sfile;

                  if ALI.Sdep.Table (D).Stamp /= Empty_Time_Stamp then
                     Dep_Src := Source_Files_Htable.Get
                       (Tree.Source_Files_HT, Sfile);
                     Found := False;

                     if Dep_Src /= No_Source then
                        Insert_Source := True;
                        for J in 1 .. Processed_Sources.Last loop
                           if Processed_Sources.Table (J) = Dep_Src then
                              Insert_Source := False;
                              exit;
                           end if;
                        end loop;

                        if Insert_Source then
                           Processed_Sources.Append (Dep_Src);
                        end if;
                     end if;

                     while Dep_Src /= No_Source loop
                        Initialize_Source_Record (Dep_Src);

                        if not Dep_Src.Locally_Removed
                          and then Dep_Src.Unit /= No_Unit_Index
                        then
                           Found := True;

                           if Opt.Minimal_Recompilation
                             and then ALI.Sdep.Table (D).Stamp /=
                             Dep_Src.Source_TS
                           then
                              --  If minimal recompilation is in action,
                              --  replace the stamp of the source file in
                              --  the table if checksums match.

                              declare
                                 Source_Index : Source_File_Index;
                                 use Scans;

                              begin
                                 Source_Index :=
                                   Sinput.Load_File
                                     (Get_Name_String
                                        (Dep_Src.Path.Display_Name));

                                 if Source_Index /= No_Source_File then

                                    Err.Scanner.Initialize_Scanner
                                      (Source_Index, Err.Scanner.Ada);

                                    --  Scan the complete file to compute its
                                    --  checksum.

                                    loop
                                       Err.Scanner.Scan;
                                       exit when Token = Tok_EOF;
                                    end loop;

                                    if Scans.Checksum =
                                      ALI.Sdep.Table (D).Checksum
                                    then
                                       ALI.Sdep.Table (D).Stamp :=
                                         Dep_Src.Source_TS;
                                    end if;
                                 end if;

                                 --  To avoid using too much memory, free the
                                 --  memory allocated.

                                 Sinput.Clear_Source_File_Table;
                              end;
                           end if;

                           if ALI.Sdep.Table (D).Stamp /= Dep_Src.Source_TS
                           then
                              if Opt.Verbosity_Level > Opt.Low then
                                 Put
                                   ("   -> different time stamp for ");
                                 Put_Line (Get_Name_String (Sfile));

                                 if Debug.Debug_Flag_T then
                                    Put ("   in ALI file: ");
                                    Put_Line
                                      (String (ALI.Sdep.Table (D).Stamp));
                                    Put ("   actual file: ");
                                    Put_Line (String (Dep_Src.Source_TS));
                                 end if;
                              end if;

                              return True;

                           --  Favor comparison against object file if possible
                           --  since object file may have been created later
                           --  than ALI file.

                           elsif Object_Check
                             and then Source.Language.Config.Object_Generated
                           then
                              if Dep_Src.Source_TS > Source.Object_TS then
                                 if Opt.Verbosity_Level > Opt.Low then
                                    Put ("   -> file ");
                                    Put
                                      (Get_Name_String
                                         (Dep_Src.Path.Display_Name));
                                    Put_Line (" more recent than object file");
                                 end if;

                                 return True;
                              end if;

                           elsif Dep_Src.Source_TS > TS0 then
                              if Opt.Verbosity_Level > Opt.Low then
                                 Put ("   -> file ");
                                 Put
                                   (Get_Name_String
                                      (Dep_Src.Path.Display_Name));
                                 Put_Line (" more recent than ALI file");
                              end if;

                              return True;

                           end if;
                        end if;

                        Dep_Src := Dep_Src.Next_With_File_Name;
                     end loop;
                  end if;
               end loop;
            end if;

            Last_Processed_Source := Last_Processed_Source + 1;
         end loop;

         return False;
      end Process_ALI_Closure_Deps;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
      begin
         Free (C_Object_Name);
         Free (Object_Path);
         Free (Switches_Name);
      end Cleanup;

   begin
      The_ALI := ALI.No_ALI_Id;

      --  Never attempt to compile header files

      if Source.Language.Config.Kind = File_Based
        and then Source.Kind = Spec
      then
         Must_Compile := False;
         return;
      end if;

      if Force_Compilations then
         Must_Compile := Always_Compile or else (not Externally_Built);
         return;
      end if;

      --  No need to compile if there is no "compiler"

      if Length_Of_Name (Source.Language.Config.Compiler_Driver) = 0 then
         Must_Compile := False;
         return;
      end if;

      if Source.Language.Config.Object_Generated and then Object_Check then
         C_Object_Name := new String'(Get_Name_String (Source.Object));
         Canonical_Case_File_Name (C_Object_Name.all);
         Object_Path := new String'(Get_Name_String (Source.Object_Path));

         if Source.Switches_Path /= No_Path then
            Switches_Name :=
              new String'(Get_Name_String (Source.Switches_Path));
         end if;
      end if;

      if Opt.Verbosity_Level > Opt.Low then
         Put  ("   Checking ");
         Put  (Source_Path);

         if Source.Index /= 0 then
            Put (" at");
            Put (Source.Index'Img);
         end if;

         Put_Line (" ... ");
      end if;

      --  No need to compile if project is externally built

      if Externally_Built then
         if Opt.Verbosity_Level > Opt.Low then
            Put_Line ("      project is externally built");
         end if;

         Must_Compile := False;
         Cleanup;
         return;
      end if;

      if not Source.Language.Config.Object_Generated then
         --  If no object file is generated, the "compiler" need to be invoked
         --  if there is no dependency file.

         if Source.Language.Config.Dependency_Kind = None then
            if Opt.Verbosity_Level > Opt.Low then
               Put_Line ("      -> no object file generated");
            end if;

            Must_Compile := True;
            Cleanup;
            return;
         end if;

      elsif Object_Check then
         --  If object file does not exist, of course source need to be
         --  compiled.

         if Source.Object_TS = Empty_Time_Stamp then
            if Opt.Verbosity_Level > Opt.Low then
               Put  ("      -> object file ");
               Put  (Object_Path.all);
               Put_Line (" does not exist");
            end if;

            Must_Compile := True;
            Cleanup;
            return;
         end if;

         --  If the object file has been created before the last modification
         --  of the source, the source need to be recompiled.

         if (not Opt.Minimal_Recompilation)
           and then Source.Object_TS < Source.Source_TS
         then
            if Opt.Verbosity_Level > Opt.Low then
               Put  ("      -> object file ");
               Put  (Object_Path.all);
               Put_Line (" has time stamp earlier than source");
            end if;

            Must_Compile := True;
            Cleanup;
            return;
         end if;

         if Opt.Verbosity_Level > Opt.Low and then Debug.Debug_Flag_T then
            Put ("   object file ");
            Put (Object_Path.all);
            Put (": ");
            Put_Line (String (Source.Object_TS));

            Put ("   source file: ");
            Put_Line (String (Source.Source_TS));
         end if;
      end if;

      if Source.Language.Config.Dependency_Kind /= None then

         --  If there is no dependency file, then the source needs to be
         --  recompiled and the dependency file need to be created.

         Stamp := File_Time_Stamp (Source.Dep_Path, Source.Dep_TS'Access);

         if Stamp = Empty_Time_Stamp then
            if Opt.Verbosity_Level > Opt.Low then
               Put  ("      -> dependency file ");
               Put  (Get_Name_String (Source.Dep_Path));
               Put_Line (" does not exist");
            end if;

            Must_Compile := True;
            Cleanup;
            return;
         end if;

         --  If the ALI file has been created after the object file, we need
         --  to recompile.

         if Object_Check
           and then
             (Source.Language.Config.Dependency_Kind = ALI_File
              or else Source.Language.Config.Dependency_Kind = ALI_Closure)
           and then
             Source.Object_TS < Stamp
         then
            if Opt.Verbosity_Level > Opt.Low then
               Put  ("      -> ALI file ");
               Put  (Get_Name_String (Source.Dep_Path));
               Put_Line (" has timestamp earlier than object file");
            end if;

            Must_Compile := True;
            Cleanup;
            return;
         end if;

         --  The source needs to be recompiled if the source has been modified
         --  after the dependency file has been created.

         if not Opt.Minimal_Recompilation
           and then Stamp < Source.Source_TS
         then
            if Opt.Verbosity_Level > Opt.Low then
               Put  ("      -> dependency file ");
               Put  (Get_Name_String (Source.Dep_Path));
               Put_Line (" has time stamp earlier than source");
            end if;

            Must_Compile := True;
            Cleanup;
            return;
         end if;
      end if;

      --  If we are checking the switches and there is no switches file, then
      --  the source needs to be recompiled and the switches file need to be
      --  created.

      if Check_Switches and then Switches_Name /= null then
         if Source.Switches_TS = Empty_Time_Stamp then
            if Opt.Verbosity_Level > Opt.Low then
               Put  ("      -> switches file ");
               Put  (Switches_Name.all);
               Put_Line (" does not exist");
            end if;

            Must_Compile := True;
            Cleanup;
            return;
         end if;

         --  The source needs to be recompiled if the source has been modified
         --  after the switches file has been created.

         if not Opt.Minimal_Recompilation
           and then Source.Switches_TS < Source.Source_TS
         then
            if Opt.Verbosity_Level > Opt.Low then
               Put  ("      -> switches file ");
               Put  (Switches_Name.all);
               Put_Line (" has time stamp earlier than source");
            end if;

            Must_Compile := True;
            Cleanup;
            return;
         end if;
      end if;

      case Source.Language.Config.Dependency_Kind is
         when None =>
            null;

         when Makefile =>
            if Process_Makefile_Deps
                 (Get_Name_String (Source.Dep_Path),
                  Get_Name_String
                    (Source.Project.Object_Directory.Display_Name))
            then
               Must_Compile := True;
               Cleanup;
               return;
            end if;

         when ALI_File =>
            if Process_ALI_Deps then
               Must_Compile := True;
               Cleanup;
               return;
            end if;

         when ALI_Closure =>
            if Process_ALI_Closure_Deps then
               Must_Compile := True;
               Cleanup;
               return;
            end if;
      end case;

      --  If we are here, then everything is OK, and we don't need
      --  to recompile.

      if (not Object_Check) and then Opt.Verbosity_Level > Opt.Low then
         Put_Line ("      -> up to date");
      end if;

      Must_Compile := False;
      Cleanup;
   end Need_To_Compile;

   ---------------------------
   -- Set_Default_Verbosity --
   ---------------------------

   procedure Set_Default_Verbosity is
      Gpr_Verbosity : String_Access := Getenv ("GPR_VERBOSITY");
   begin
      if Gpr_Verbosity /= null and then Gpr_Verbosity'Length > 0 then
         declare
            Verbosity : String := Gpr_Verbosity.all;
         begin
            To_Lower (Verbosity);

            if Verbosity = "quiet" then
               Quiet_Output    := True;
               Verbose_Mode    := False;
               Verbosity_Level := Opt.None;

            elsif Verbosity = "default" then
               Quiet_Output    := False;
               Verbose_Mode    := False;
               Verbosity_Level := Opt.None;

            elsif Verbosity = "verbose" or else Verbosity = "verbose_low" then
               Quiet_Output    := False;
               Verbose_Mode    := True;
               Verbosity_Level := Opt.Low;

            elsif Verbosity = "verbose_medium" then
               Quiet_Output    := False;
               Verbose_Mode    := True;
               Verbosity_Level := Opt.Medium;

            elsif Verbosity = "verbose_high" then
               Quiet_Output    := False;
               Verbose_Mode    := True;
               Verbosity_Level := Opt.High;
            end if;
         end;
      end if;

      Free (Gpr_Verbosity);
   end Set_Default_Verbosity;

   ---------------
   -- Knowledge --
   ---------------

   package body Knowledge is separate;

   --------------
   -- UTC_Time --
   --------------

   function UTC_Time return Time_Stamp_Type is
      Now : constant Time := Clock - Duration (UTC_Time_Offset) * 60;
      --  The UTC_Time_Offset is in minutes
   begin
      return Time_Stamp_Type (Image (Now, "%Y%m%d%H%M%S"));
   end UTC_Time;

   ----------------
   -- Check_Diff --
   ----------------

   function Check_Diff
     (Ts1, Ts2 : Time_Stamp_Type; Max_Drift : Duration := 5.0) return Boolean
   is
      use GNAT.Calendar;

      function Get (T : String) return Time is
        (Time_Of
           (Year   => Year_Number'Value   (T (T'First .. T'First + 3)),
            Month  => Month_Number'Value  (T (T'First + 4 .. T'First + 5)),
            Day    => Day_Number'Value    (T (T'First + 6 .. T'First + 7)),
            Hour   => Hour_Number'Value   (T (T'First + 8 .. T'First + 9)),
            Minute => Minute_Number'Value (T (T'First + 10 .. T'First + 11)),
            Second => Second_Number'Value (T (T'First + 12 .. T'First + 13))));

      T1 : constant Time := Get (String (Ts1));
      T2 : constant Time := Get (String (Ts2));

   begin
      return abs (T1 - T2) <= Max_Drift;
   end Check_Diff;

   -------------------
   -- To_Time_Stamp --
   -------------------

   function To_Time_Stamp
     (Time : Calendar.Time) return Stamps.Time_Stamp_Type is
   begin
      return Time_Stamp_Type (Image (Time, "%Y%m%d%H%M%S"));
   end To_Time_Stamp;

   package body Project_Output is
      ----------------
      -- Write_Char --
      ----------------
      procedure Write_A_Char (C : Character) is
      begin
         Write_A_String ((1 => C));
      end Write_A_Char;

      ---------------
      -- Write_Eol --
      ---------------

      procedure Write_Eol is
      begin
         Write_A_String ((1 => ASCII.LF));
      end Write_Eol;

      --------------------
      -- Write_A_String --
      --------------------

      procedure Write_A_String (S : String) is
         Str : String (1 .. S'Length);

      begin
         if S'Length > 0 then
            Str := S;

            if Write (Output_FD, Str (1)'Address, Str'Length) /= Str'Length
            then
               GPR.Com.Fail ("disk full");
            end if;
         end if;
      end Write_A_String;

   end Project_Output;

end Gpr_Util;
