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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;

pragma Warnings (Off);
with System;
with System.Case_Util;       use System.Case_Util;
with System.Multiprocessors; use System.Multiprocessors;
pragma Warnings (On);

with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Gpr_Build_Util;             use Gpr_Build_Util;
with Gpr_Script;                 use Gpr_Script;
with Gpr_Util;                   use Gpr_Util;
with Gprbuild.Compile;
with Gprbuild.Link;
with Gprbuild.Post_Compile;
with Gprbuild.Compilation.Process.Waiter;
with Gprbuild.Compilation.Slave;
with GPR;                        use GPR;
with GPR.Debug;                  use GPR.Debug;
with GPR_Version;                use GPR_Version;
with GPR.Conf;                   use GPR.Conf;
with GPR.Names;                  use GPR.Names;
with GPR.Osint;                  use GPR.Osint;
with GPR.Output;                 use GPR.Output;
with GPR.Proc;                   use GPR.Proc;
with GPR.Env;
with GPR.Err;
with GPR.Opt;                    use GPR.Opt;
with GPR.Snames;                 use GPR.Snames;
with GPR.Tempdir;                use GPR.Tempdir;
with GPR.Tree;                   use GPR.Tree;
with GPR.Util;                   use GPR.Util;

procedure Gprbuild.Main is

   use Stamps;

   use Gpr_Util.Knowledge;

   CodePeer_String : constant String := "codepeer";
   --  Used in CopePeer mode for the target and the subdirs

   Dumpmachine : constant String := "--dumpmachine";
   --  Switch to display the normalized hostname

   Dash_A_Warning : constant String :=
     "warning: switch -a is ignored and no additional source is compiled";
   --  Warning issued when gprbuild is invoked with switch -a

   Dash_A_Warning_Issued : Boolean := False;
   --  Flag used to avoid issuing the several times the warning for switch -a

   Subst_Switch_Present : Boolean := False;
   --  True if --compiler-subst=... or --compiler-pkg-subst=... appears on the
   --  command line. Used to detect switches that are incompatible with these.
   --  Also used to prevent passing builder args to the "compiler".

   Main_On_Command_Line : Boolean := False;
   --  True if there is at least one main specified on the command line

   procedure Initialize;
   --  Do the necessary package intialization and process the command line
   --  arguments.

   procedure Usage;
   --  Display the usage

   function Add_Global_Switches
     (Switch                          : String;
      For_Lang                        : Name_Id;
      For_Builder                     : Boolean;
      Has_Global_Compilation_Switches : Boolean) return Boolean;
   --  Take into account a global switch (builder or global compilation switch)
   --  read from the project file.

   procedure Add_Mains_To_Queue;
   --  Check that each main is a single file name and that it is a source
   --  of a project from the tree.

   procedure Scan_Arg
     (Arg          : String;
      Command_Line : Boolean;
      Language     : Name_Id;
      Success      : out Boolean);
   --  Process one gprbuild argument Arg. Command_Line is True if the argument
   --  is specified on the command line. Optional parameter Additional gives
   --  additional information about the origin of the argument if it is found
   --  illegal.

   procedure Add_Option (Arg : String; Command_Line : Boolean);
   --  Add a switch for a compiler or all compilers, or for the binder or for
   --  the linker. The table where this option is stored depends on the value
   --  of Current_Processor and other global variables.

   procedure Copyright;
   --  Output the Copyright notice

   type Sigint_Handler is access procedure;
   pragma Convention (C, Sigint_Handler);

   procedure Install_Int_Handler (Handler : Sigint_Handler);
   pragma Import (C, Install_Int_Handler, "__gnat_install_int_handler");
   --  Called by Gnatmake to install the SIGINT handler below

   procedure Sigint_Intercepted;
   pragma Convention (C, Sigint_Intercepted);
   --  Called when the program is interrupted by Ctrl-C to delete the
   --  temporary mapping files and configuration pragmas files.

   No_Object_Check_Switch     : constant String := "--no-object-check";
   Direct_Import_Only_Switch  : constant String := "--direct-import-only";
   Indirect_Imports_Switch    : constant String := "--indirect-imports";
   No_Indirect_Imports_Switch : constant String := "--no-indirect-imports";

   Current_Working_Dir : constant String := Get_Current_Dir;
   --  The current working directory

   type Processor is (None, Linker, Binder, Compiler);
   Current_Processor : Processor := None;
   --  This variable changes when switches -*args are used

   Current_Builder_Comp_Option_Table : Builder_Comp_Option_Table_Ref :=
                                         No_Builder_Comp_Option_Table;

   -------------------------------------------
   -- Options specified on the command line --
   -------------------------------------------

   package Options is

      type Option_Type is
        (Force_Compilations_Option,
         Keep_Going_Option,
         Maximum_Processes_Option,
         Quiet_Output_Option,
         Check_Switches_Option,
         Verbose_Mode_Option,
         Verbose_Low_Mode_Option,
         Verbose_Medium_Mode_Option,
         Verbose_High_Mode_Option,
         Warnings_Treat_As_Error,
         Warnings_Normal,
         Warnings_Suppress,
         Indirect_Imports);

      procedure Register_Command_Line_Option
        (Option : Option_Type; Value : Natural := 0);
      --  Record a command line option

      procedure Process_Command_Line_Options;
      --  Reprocess the recorded command line options that have priority over
      --  the options in package Builder of the main project.

   end Options;

   use Options;

   ------------------------
   -- Add_Mains_To_Queue --
   ------------------------

   procedure Add_Mains_To_Queue is
      Main_Id : Main_Info;

   begin
      Mains.Reset;

      loop
         Main_Id := Mains.Next_Main;
         exit when Main_Id = No_Main_Info;

         if Main_Id.Source /= No_Source then
            --  Fail if any main is declared as an excluded source file

            if Main_Id.Source.Locally_Removed then
               Fail_Program
                 (Project_Tree,
                  "main """ &
                  Get_Name_String (Main_Id.Source.File) &
                  """ cannot also be an excluded file");
            end if;

            if Is_Allowed_Language (Main_Id.Source.Language.Name) then
               Queue.Insert
                 (Source     => (Tree    => Main_Id.Tree,
                                 Id      => Main_Id.Source,
                                 Closure => False),
                  With_Roots => Builder_Data (Main_Id.Tree).Closure_Needed);

               --  If a non Ada main has no roots, then all sources need to be
               --  compiled, so no need to check for closure.

               if Main_Id.Source.Language.Config.Kind /= Unit_Based
                 and then Main_Id.Source.Roots = null
               then
                  Builder_Data (Main_Id.Tree).Closure_Needed := False;
               end if;
            end if;
         end if;
      end loop;

      if Total_Errors_Detected /= 0 then
         Fail_Program (Project_Tree, "cannot continue");
      end if;

      --  If the main project is an aggregated project and there is at least
      --  one main on the command line, do not add the sources of the projects
      --  without mains to the queue.

      if Main_Project.Qualifier = Aggregate and then Main_On_Command_Line then
         Mains.Reset;

         loop
            Main_Id := Mains.Next_Main;
            exit when Main_Id = No_Main_Info;

            Queue.Insert_Project_Sources
              (Project        => Main_Id.Project,
               Project_Tree   => Main_Id.Tree,
               Unique_Compile => Unique_Compile,
               All_Projects =>
                  not Unique_Compile
               or else (Unique_Compile_All_Projects or Recursive));
         end loop;

      else
         Queue.Insert_Project_Sources
           (Project        => Main_Project,
            Project_Tree   => Project_Tree,
            Unique_Compile => Unique_Compile,
            All_Projects =>
               not Unique_Compile
            or else (Unique_Compile_All_Projects or Recursive));
      end if;
   end Add_Mains_To_Queue;

   -------------------------
   -- Add_Global_Switches --
   -------------------------

   function Add_Global_Switches
     (Switch                          : String;
      For_Lang                        : Name_Id;
      For_Builder                     : Boolean;
      Has_Global_Compilation_Switches : Boolean) return Boolean
   is
      Success : Boolean;
   begin
      if For_Builder then
         if Has_Global_Compilation_Switches then
            Builder_Switches_Lang := No_Name;
         else
            Builder_Switches_Lang := For_Lang;
         end if;

         Scan_Arg
           (Switch,
            Command_Line => False,
            Language     => For_Lang,
            Success      => Success);
         return Success;

      else
         Current_Processor := Compiler;

         Current_Builder_Comp_Option_Table :=
           Builder_Compiling_Options_HTable.Get (For_Lang);

         if Current_Builder_Comp_Option_Table =
           No_Builder_Comp_Option_Table
         then
            Current_Builder_Comp_Option_Table :=
              new Builder_Compiling_Options.Instance;
            Builder_Compiling_Options_HTable.Set
              (For_Lang, Current_Builder_Comp_Option_Table);
            Builder_Compiling_Options.Init
              (Current_Builder_Comp_Option_Table.all);
         end if;

         Add_Option (Switch, Command_Line => False);

         Current_Processor := None;
         return True;
      end if;
   end Add_Global_Switches;

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Arg : String; Command_Line : Boolean) is
      Option : String_Access := new String'(Arg);

   begin
      case Current_Processor is
         when None =>
            null;

         when Linker =>

            --  Add option to the linker table

            if Command_Line then
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Current_Working_Dir,
                  Including_Switch => Dash_L);

            else
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Main_Project_Dir.all,
                  Including_Switch => Dash_L);
            end if;

            Command_Line_Linker_Options.Append (Option);

         when Binder =>

            if Command_Line then
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Current_Working_Dir,
                  Including_Switch => No_Name);

            else
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Main_Project_Dir.all,
                  Including_Switch => No_Name);
            end if;

            if Current_Bind_Option_Table = No_Bind_Option_Table then
               --  Option for all binder

               All_Language_Binder_Options.Append (Option);

            else
               --  Option for a single binder

               Binder_Options.Append
                 (Current_Bind_Option_Table.all, Option);
            end if;

         when Compiler =>

            if Command_Line then
               if Current_Comp_Option_Table = No_Comp_Option_Table then
                  --  Option for all compilers

                  All_Language_Compiling_Options.Append (Option);

               else
                  --  Option for a single compiler

                  Compiling_Options.Append
                    (Current_Comp_Option_Table.all, Option);
               end if;

            else
               if Current_Builder_Comp_Option_Table =
                    No_Builder_Comp_Option_Table
               then
                  --  Option for all compilers

                  All_Language_Builder_Compiling_Options.Append (Option);

               else
                  --  Option for a single compiler

                  Builder_Compiling_Options.Append
                    (Current_Builder_Comp_Option_Table.all, Option);
               end if;
            end if;
      end case;
   end Add_Option;

   ---------------
   -- Copyright --
   ---------------

   procedure Copyright is
   begin
      --  Only output the Copyright notice once

      if not Copyright_Output then
         Copyright_Output := True;
         Display_Version
           ("GPRBUILD", "2004", Version_String => Gpr_Version_String);
      end if;
   end Copyright;

   -------------
   -- Options --
   -------------

   package body Options is

      type Option_Data is record
         Option : Option_Type;
         Value  : Natural := 0;
      end record;

      package Command_Line_Options is new GNAT.Table
        (Table_Component_Type => Option_Data,
         Table_Index_Type     => Natural,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100);
      --  Table to store the command line options

      ----------------------------------
      -- Process_Command_Line_Options --
      ----------------------------------

      procedure Process_Command_Line_Options is
      begin
         for Index in 1 .. Command_Line_Options.Last loop
            case Command_Line_Options.Table (Index).Option is
               when Force_Compilations_Option =>
                  Opt.Force_Compilations := True;

               when Keep_Going_Option =>
                  Opt.Keep_Going := True;

               when Maximum_Processes_Option =>
                  Opt.Maximum_Processes :=
                    Command_Line_Options.Table (Index).Value;

               when Quiet_Output_Option =>
                  Opt.Quiet_Output    := True;
                  Opt.Verbose_Mode    := False;
                  Opt.Verbosity_Level := Opt.None;

               when Check_Switches_Option =>
                  Opt.Check_Switches := True;

               when Verbose_Mode_Option =>
                  Opt.Verbose_Mode    := True;
                  Opt.Verbosity_Level := Opt.Low;
                  Opt.Quiet_Output    := False;

               when Verbose_Low_Mode_Option =>
                  Opt.Verbose_Mode    := True;
                  Opt.Verbosity_Level := Opt.Low;
                  Opt.Quiet_Output    := False;

               when Verbose_Medium_Mode_Option =>
                  Opt.Verbose_Mode    := True;
                  Opt.Verbosity_Level := Opt.Medium;
                  Opt.Quiet_Output    := False;

               when Verbose_High_Mode_Option =>
                  Opt.Verbose_Mode    := True;
                  Opt.Verbosity_Level := Opt.High;
                  Opt.Quiet_Output    := False;

               when Warnings_Treat_As_Error =>
                  Opt.Warning_Mode := Opt.Treat_As_Error;

               when Warnings_Normal =>
                  Opt.Warning_Mode := Opt.Normal;

               when Warnings_Suppress =>
                  Opt.Warning_Mode := Opt.Suppress;

               when Indirect_Imports =>
                  Gprbuild.Indirect_Imports :=
                    Command_Line_Options.Table (Index).Value /= 0;
            end case;
         end loop;
      end Process_Command_Line_Options;

      ----------------------------------
      -- Register_Command_Line_Option --
      ----------------------------------

      procedure Register_Command_Line_Option
        (Option : Option_Type; Value : Natural := 0)
      is
      begin
         Command_Line_Options.Increment_Last;
         Command_Line_Options.Table (Command_Line_Options.Last) :=
           (Option => Option, Value => Value);
      end Register_Command_Line_Option;

   end Options;

   --------------
   -- Scan_Arg --
   --------------

   procedure Scan_Arg
     (Arg          : String;
      Command_Line : Boolean;
      Language     : Name_Id;
      Success      : out Boolean)
   is
      Processed : Boolean := True;

      procedure Forbidden_In_Package_Builder;
      --  Fail if switch Arg is found in package Builder

      procedure Forbidden_With_Subst;
      --  Fail if an incompatibility with -compiler-[pkg]-subst is found

      ----------------------------------
      -- Forbidden_In_Package_Builder --
      ----------------------------------

      procedure Forbidden_In_Package_Builder is
      begin
         if not Command_Line then
            Fail_Program
              (Project_Tree,
               Arg & " can only be used on the command line");
         end if;
      end Forbidden_In_Package_Builder;

      ----------------------------------
      -- Forbidden_With_Subst --
      ----------------------------------

      procedure Forbidden_With_Subst is
      begin
         if Subst_Switch_Present then
            Fail_Program
              (Project_Tree,
               Arg & " is not compatible with compiler-[pkg-]subst");
         end if;
      end Forbidden_With_Subst;

   begin
      pragma Assert (Arg'First = 1);

      Success := True;

      if Arg'Length = 0 then
         return;
      end if;

      --  If preceding switch was -P, a project file name need to be
      --  specified, not a switch.

      if Project_File_Name_Expected then
         if Arg (1) = '-' then
            Fail_Program
              (Project_Tree, "project file name missing after -P");
         else
            Project_File_Name_Expected := False;
            Project_File_Name := new String'(Arg);
         end if;

         --  If preceding switch was -o, an executable name need to be
         --  specified, not a switch.

      elsif Output_File_Name_Expected then
         if Arg (1) = '-' then
            Fail_Program
              (Project_Tree, "output file name missing after -o");
         else
            Output_File_Name_Expected := False;
            Output_File_Name := new String'(Arg);
         end if;

      elsif Search_Project_Dir_Expected then
         if Arg (1) = '-' then
            Fail_Program
              (Project_Tree, "directory name missing after -aP");
         else
            Search_Project_Dir_Expected := False;
            GPR.Env.Add_Directories (Root_Environment.Project_Path, Arg);
         end if;

      elsif Db_Directory_Expected then
         Db_Directory_Expected := False;
         Parse_Knowledge_Base (Project_Tree, Arg);

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Arg);
         Add_Db_Switch_Arg (Name_Find);

         --  Set the processor/language for the following switches

         --  -cargs         all compiler arguments

      elsif Arg = "-cargs" then
         Current_Processor := Compiler;

         if Command_Line then
            Current_Comp_Option_Table := No_Comp_Option_Table;

         else
            Current_Builder_Comp_Option_Table := No_Builder_Comp_Option_Table;
         end if;

         --  -cargs:lang    arguments for compiler of language lang

      elsif Arg'Length > 7 and then Arg (1 .. 7) = "-cargs:" then
         Current_Processor := Compiler;

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Arg (8 .. Arg'Last));
         To_Lower (Name_Buffer (1 .. Name_Len));

         declare
            Lang : constant Name_Id := Name_Find;
         begin
            if Command_Line then
               Current_Comp_Option_Table :=
                 Compiling_Options_HTable.Get (Lang);

               if Current_Comp_Option_Table = No_Comp_Option_Table then
                  Current_Comp_Option_Table := new Compiling_Options.Instance;
                  Compiling_Options_HTable.Set
                    (Lang, Current_Comp_Option_Table);
                  Compiling_Options.Init (Current_Comp_Option_Table.all);
               end if;

            else
               Current_Builder_Comp_Option_Table :=
                 Builder_Compiling_Options_HTable.Get (Lang);

               if Current_Builder_Comp_Option_Table =
                 No_Builder_Comp_Option_Table
               then
                  Current_Builder_Comp_Option_Table :=
                    new Builder_Compiling_Options.Instance;
                  Builder_Compiling_Options_HTable.Set
                    (Lang, Current_Builder_Comp_Option_Table);
                  Builder_Compiling_Options.Init
                    (Current_Builder_Comp_Option_Table.all);
               end if;
            end if;
         end;

         --  -bargs     all binder arguments

      elsif Arg = "-bargs" then
         Forbidden_With_Subst;
         Current_Processor := Binder;
         Current_Bind_Option_Table := No_Bind_Option_Table;

         --  -bargs:lang    arguments for binder of language lang

      elsif Arg'Length > 7 and then Arg (1 .. 7) = "-bargs:" then
         Forbidden_With_Subst;
         Current_Processor := Binder;

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Arg (8 .. Arg'Last));
         To_Lower (Name_Buffer (1 .. Name_Len));

         declare
            Lang : constant Name_Id := Name_Find;
         begin
            Current_Bind_Option_Table :=
              Binder_Options_HTable.Get (Lang);

            if Current_Bind_Option_Table = No_Bind_Option_Table then
               Current_Bind_Option_Table := new Binder_Options.Instance;
               Binder_Options_HTable.Set
                 (Lang, Current_Bind_Option_Table);
               Binder_Options.Init (Current_Bind_Option_Table.all);
            end if;
         end;

         --  -largs     linker arguments

      elsif Arg = "-largs" then
         Forbidden_With_Subst;
         Current_Processor := Linker;

         --  -gargs/margs     options directly for gprbuild
         --  support -margs for compatibility with gnatmake

      elsif Arg = "-gargs"
        or else Arg = "-margs"
      then
         Current_Processor := None;

         --  A special test is needed for the -o switch within a -largs since
         --  that is another way to specify the name of the final executable.

      elsif Command_Line
        and then Current_Processor = Linker
        and then Arg = "-o"
      then
            Fail_Program
           (Project_Tree,
            "switch -o not allowed within a -largs. Use -o directly.");

         --  If current processor is not gprbuild directly, store the option
         --  in the appropriate table.

      elsif Current_Processor /= None then
         Add_Option (Arg, Command_Line);

         --  Switches start with '-'

      elsif Arg (1) = '-' then

         if Arg = Complete_Output_Option then
            Forbidden_In_Package_Builder;

            if Distributed_Mode then
               Fail_Program
                 (Project_Tree,
                  "options " & Complete_Output_Option &
                    Distributed_Option & " are not compatible");
            end if;

            Complete_Output    := True;
            No_Complete_Output := False;

         elsif Arg = No_Complete_Output_Option or else Arg = "-n" then
            Forbidden_In_Package_Builder;
            No_Complete_Output := True;
            Complete_Output := False;

         elsif Arg'Length >= Distributed_Option'Length
            and then
            Arg (1 .. Distributed_Option'Length) = Distributed_Option
         then
            Forbidden_With_Subst;

            if Complete_Output then
               Fail_Program
                 (Project_Tree,
                  "options " & Complete_Output_Option &
                    Distributed_Option & " are not compatible");
            end if;

            if Build_Script_Name /= null then
               Fail_Program
                 (Project_Tree,
                  "options " & Build_Script_Option &
                    Distributed_Option & " are not compatible");
            end if;

            Distributed_Mode := True;

            --  In distributed mode we do not want to use temp directories

            Use_Temp_Dir (Status => False);

            declare
               Hosts : constant String := Get_Slaves_Hosts (Project_Tree, Arg);
            begin
               if Hosts = "" then
                  Fail_Program
                    (Project_Tree,
                     "missing hosts for distributed mode compilation");

               else
                  Compilation.Slave.Record_Slaves (Hosts);
               end if;
            end;

         elsif Arg'Length >= Hash_Option'Length
            and then Arg (1 .. Hash_Option'Length) = Hash_Option
         then
            Forbidden_With_Subst;

            Hash_Value :=
              new String'(Arg (Hash_Option'Length + 2 .. Arg'Last));

         elsif Arg'Length >= Slave_Env_Option'Length
            and then
            Arg (1 .. Slave_Env_Option'Length) = Slave_Env_Option
         then
            Forbidden_With_Subst;

            if Arg = Slave_Env_Option then
               --  Just --slave-env, it is up to gprbuild to build a sensible
               --  slave environment value.
               Slave_Env_Auto := True;
            else
               Slave_Env :=
                 new String'(Arg (Slave_Env_Option'Length + 2 .. Arg'Last));
            end if;

         elsif Arg'Length >= Compiler_Subst_Option'Length
            and then
            Arg (1 .. Compiler_Subst_Option'Length) = Compiler_Subst_Option
         then
            Forbidden_In_Package_Builder;

            --  We should have Arg set to something like:
            --     "compiler-subst=ada,gnatpp".
            --  We need to pick out the "ada" and "gnatpp".

            declare
               function Scan_To_Comma (Start : Positive) return Positive;
               --  Scan forward from Start until we find a comma or end of
               --  string. Return the index just before the ",", or Arg'Last.

               function Scan_To_Comma (Start : Positive) return Positive is
               begin
                  if Start >= Arg'Last then
                     return Arg'Last;
                  end if;

                  return Result : Positive := Start do
                     while Result < Arg'Last
                       and then Arg (Result + 1) /= ','
                     loop
                        Result := Result + 1;
                     end loop;
                  end return;
               end Scan_To_Comma;

               Lang_Start : constant Positive :=
                 Compiler_Subst_Option'Length + 1;
               Lang_End : constant Positive := Scan_To_Comma (Lang_Start);
               Comp_Start : constant Positive := Lang_End + 2;
               Comp_End : constant Positive := Scan_To_Comma (Comp_Start);

               Lang : String renames Arg (Lang_Start .. Lang_End);
               Comp : String renames Arg (Comp_Start .. Comp_End);

            begin
               if Lang = "" or else Comp = "" then
                  Fail_Program (Project_Tree, "invalid switch " & Arg);
                  --  This switch is intended for internal use by ASIS tools,
                  --  so a friendlier error message isn't needed here.
               end if;

               Name_Len := 0;
               Add_Str_To_Name_Buffer (Lang);
               To_Lower (Name_Buffer (1 .. Name_Len));

               declare
                  Lang_Id : constant Name_Id := Name_Find;
               begin
                  Name_Len := 0;
                  Add_Str_To_Name_Buffer (Comp);

                  declare
                     Comp_Id : constant Name_Id := Name_Find;
                  begin
                     Compiler_Subst_HTable.Set (Lang_Id, Comp_Id);
                  end;
               end;
            end;

         elsif Arg'Length >= Compiler_Pkg_Subst_Option'Length
            and then
            Arg (1 .. Compiler_Pkg_Subst_Option'Length) =
              Compiler_Pkg_Subst_Option
         then
            Forbidden_In_Package_Builder;

            declare
               Package_Name : String renames
                 Arg (Compiler_Pkg_Subst_Option'Length + 1 .. Arg'Last);
            begin
               if Package_Name = "" then
                  Fail_Program (Project_Tree, "invalid switch " & Arg);
                  --  This switch is intended for internal use by ASIS tools,
                  --  so a friendly error message isn't needed here.
                  --  No error if the package doesn't exist; gnatpp might pass
                  --  --compiler-pkg-subst=pretty_printer even when there is no
                  --  package Pretty_Printer in the project file.
               end if;

               Name_Len := 0;
               Add_Str_To_Name_Buffer (Package_Name);
               To_Lower (Name_Buffer (1 .. Name_Len));
               Compiler_Pkg_Subst := Name_Find;
            end;

         elsif Arg'Length > Build_Script_Option'Length
           and then
             Arg (1 .. Build_Script_Option'Length) = Build_Script_Option
         then
            Forbidden_In_Package_Builder;

            if Distributed_Mode then
               Fail_Program
                 (Project_Tree,
                  "options " & Build_Script_Option &
                    Distributed_Option & " are not compatible");
            end if;

            declare
               Script_Name : constant String :=
                 Arg (Build_Script_Option'Length + 1 .. Arg'Last);
            begin
               if Is_Absolute_Path (Script_Name) then
                  Build_Script_Name := new String'(Script_Name);

               else
                  Build_Script_Name :=
                    new String'
                      (Ada.Directories.Current_Directory & '/' & Script_Name);
               end if;
            end;

         elsif Arg = "--db-" then
            Forbidden_In_Package_Builder;

            Load_Standard_Base := False;

         elsif Arg = "--db" then
            Forbidden_In_Package_Builder;

            Db_Directory_Expected := True;

         elsif Arg = "--display-paths" then
            Forbidden_In_Package_Builder;
            Display_Paths := True;

         elsif Arg = "--no-split-units" then
            Opt.No_Split_Units := True;

         elsif Arg = Single_Compile_Per_Obj_Dir_Switch then
            Opt.One_Compilation_Per_Obj_Dir := True;

         elsif Arg'Length > Source_Info_Option'Length
           and then Arg (1 .. Source_Info_Option'Length) = Source_Info_Option
         then
            Forbidden_In_Package_Builder;
            Project_Tree.Source_Info_File_Name :=
               new String'(Arg (Source_Info_Option'Length + 1 .. Arg'Last));

         elsif Arg'Length > Config_Project_Option'Length
           and then
               Arg (1 .. Config_Project_Option'Length) = Config_Project_Option
         then
            if Config_Project_File_Name /= null
              and then Command_Line
              and then (Autoconf_Specified
                        or else Config_Project_File_Name.all /=
                          Arg (Config_Project_Option'Length + 1 .. Arg'Last))
            then
               Fail_Program
                 (Project_Tree,
                  "several different configuration switches " &
                  "cannot be specified");

            else
               Autoconfiguration := False;
               Autoconf_Specified := False;
               Config_Project_File_Name :=
                 new String'
                   (Arg (Config_Project_Option'Length + 1 .. Arg'Last));
            end if;

         elsif Arg'Length > Autoconf_Project_Option'Length
           and then
            Arg (1 .. Autoconf_Project_Option'Length) =
              Autoconf_Project_Option
         then
            Forbidden_In_Package_Builder;

            if Config_Project_File_Name /= null
              and then (not Autoconf_Specified
                        or else Config_Project_File_Name.all /=
                          Arg (Autoconf_Project_Option'Length + 1 .. Arg'Last))
            then
               Fail_Program
                 (Project_Tree,
                  "several different configuration switches " &
                  "cannot be specified");

            else
               Config_Project_File_Name :=
                 new String'
                   (Arg (Autoconf_Project_Option'Length + 1 .. Arg'Last));
               Autoconf_Specified := True;
            end if;

         elsif Arg'Length > Target_Project_Option'Length
                 and then
               Arg (1 .. Target_Project_Option'Length) = Target_Project_Option
         then
            Forbidden_In_Package_Builder;

            if CodePeer_Mode then
               --  In CodePeer mode, the target is always the native one,
               --  so no target may be specified.

               Fail_Program
                 (Project_Tree,
                  "target cannot be specified in CodePeer mode");

            elsif Target_Name /= null then
               if Target_Name.all /=
                 Arg (Target_Project_Option'Length + 1 .. Arg'Last)
               then
                  Fail_Program
                    (Project_Tree,
                     "several different target switches cannot be specified");
               end if;

            else
               Target_Name :=
                 new String'
                   (Arg (Target_Project_Option'Length + 1 .. Arg'Last));
            end if;

         elsif Arg'Length > RTS_Option'Length
           and then Arg (1 .. RTS_Option'Length) = RTS_Option
         then
            declare
               Set : constant Boolean := Runtime_Name_Set_For (Name_Ada);
               Old : constant String := Runtime_Name_For (Name_Ada);
               RTS : constant String :=
                        Arg (RTS_Option'Length + 1 .. Arg'Last);
            begin
               if Command_Line then
                  if Set and then Old /= RTS then
                     Fail_Program
                       (Project_Tree,
                        "several different run-times cannot be specified");
                  end if;

                  Set_Runtime_For (Name_Ada, RTS);
                  Set_Default_Runtime_For (Name_Ada, RTS);
               end if;

               --  Ignore any --RTS= switch in package Builder. These are only
               --  taken into account to create the config file in
               --  auto-configuration.
            end;

         elsif Arg'Length > RTS_Language_Option'Length
           and then Arg (1 .. RTS_Language_Option'Length) = RTS_Language_Option
         then
            declare
               Language_Name : Name_Id := No_Name;
               RTS_Start : Natural := Arg'Last + 1;

            begin
               for J in RTS_Language_Option'Length + 2 .. Arg'Last loop
                  if Arg (J) = '=' then
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer
                       (Arg (RTS_Language_Option'Length + 1 .. J - 1));
                     To_Lower (Name_Buffer (1 .. Name_Len));
                     Language_Name := Name_Find;
                     RTS_Start := J + 1;
                     exit;
                  end if;
               end loop;

               if Language_Name = No_Name then
                  Fail_Program (Project_Tree, "illegal switch: " & Arg);

               elsif Command_Line then
                  --  Ignore any --RTS:<lang>= switch in package Builder. These
                  --  are only taken into account to create the config file in
                  --  auto-configuration.

                  declare
                     RTS : constant String := Arg (RTS_Start .. Arg'Last);
                     Set : constant Boolean :=
                       Runtime_Name_Set_For (Language_Name);
                     Old : constant String := Runtime_Name_For (Language_Name);

                  begin
                     if Set and then Old /= RTS then
                        Fail_Program
                          (Project_Tree,
                           "several different run-times cannot be specified" &
                           " for the same language");

                     else
                        Set_Runtime_For (Language_Name, RTS);
                        Set_Default_Runtime_For (Language_Name, RTS);
                     end if;
                  end;
               end if;
            end;

         elsif Arg'Length > Subdirs_Option'Length
           and then Arg (1 .. Subdirs_Option'Length) = Subdirs_Option
         then
            Forbidden_In_Package_Builder;
            Subdirs :=
              new String'(Arg (Subdirs_Option'Length + 1 .. Arg'Last));

         elsif Arg'Length >= Relocate_Build_Tree_Option'Length
           and then Arg (1 .. Relocate_Build_Tree_Option'Length)
                    = Relocate_Build_Tree_Option
         then
            Forbidden_In_Package_Builder;

            if Arg'Length = Relocate_Build_Tree_Option'Length then
               Build_Tree_Dir := new String'(Current_Working_Dir);

            else
               Build_Tree_Dir :=
                 new String'
                   (Normalize_Pathname
                      (Arg (Relocate_Build_Tree_Option'Length + 2 .. Arg'Last),
                       Current_Working_Dir) & Directory_Separator);
            end if;

            --  Out-of-tree compilation also imply -p (create missing dirs)

            Opt.Setup_Projects := True;

         elsif Arg'Length >= Root_Dir_Option'Length
           and then Arg (1 .. Root_Dir_Option'Length) = Root_Dir_Option
         then
            Forbidden_In_Package_Builder;

            Root_Dir :=
              new String'
                (Normalize_Pathname
                   (Arg (Root_Dir_Option'Length + 2 .. Arg'Last),
                    Current_Working_Dir)
                & Dir_Separator);

         elsif Command_Line and then Arg = "--no-sal-binding" then
            No_SAL_Binding := True;

         elsif Command_Line
           and then Arg'Length > Restricted_To_Languages_Option'Length
           and then Arg (1 .. Restricted_To_Languages_Option'Length) =
                      Restricted_To_Languages_Option
         then
            declare
               Start  : Positive := Restricted_To_Languages_Option'Length + 1;
               Finish : Positive;

            begin
               Processed := False;

               while Start <= Arg'Last loop
                  Finish := Start;
                  loop
                     exit when Finish > Arg'Last or else Arg (Finish) = ',';
                     Finish := Finish + 1;
                  end loop;

                  if Finish > Start then
                     Add_Restricted_Language (Arg (Start .. Finish - 1));
                     Processed := True;
                  end if;

                  Start := Finish + 1;
               end loop;
            end;

         elsif Arg = Indirect_Imports_Switch then
            Indirect_Imports := True;

            if Command_Line then
               Register_Command_Line_Option (Options.Indirect_Imports, 1);
            end if;

         elsif Arg = No_Indirect_Imports_Switch
               or else
               Arg = Direct_Import_Only_Switch
         then
            Indirect_Imports := False;

            if Command_Line then
               Register_Command_Line_Option (Options.Indirect_Imports, 0);
            end if;

         elsif Arg = Gpr_Build_Util.Unchecked_Shared_Lib_Imports then
            Forbidden_In_Package_Builder;
            Opt.Unchecked_Shared_Lib_Imports := True;

         elsif Arg = No_Object_Check_Switch then
            Object_Checked := False;

         elsif Arg = No_Exit_Message_Option then
            Opt.No_Exit_Message := True;

         elsif Arg = "--codepeer" then
            Forbidden_In_Package_Builder;

            if Target_Name /= null
              and then Target_Name.all /= CodePeer_String
            then
               --  In CodePeer mode, the target is always the native one,
               --  so no target may be specified.

               Fail_Program
                 (Project_Tree,
                  "target cannot be specified in CodePeer mode");

            elsif not CodePeer_Mode then
               CodePeer_Mode := True;
               Object_Checked := False;
               Target_Name := new String'(CodePeer_String);

               if Subdirs = null then
                  Subdirs := new String'(CodePeer_String);
               end if;
            end if;

         elsif Arg = Create_Map_File_Switch then
            Forbidden_With_Subst;
            Map_File := new String'("");

         elsif Arg'Length > Create_Map_File_Switch'Length + 1
           and then
             Arg (1 .. Create_Map_File_Switch'Length) = Create_Map_File_Switch
           and then
             Arg (Create_Map_File_Switch'Length + 1) = '='
         then
            Forbidden_With_Subst;
            Map_File :=
              new String'(Arg (Create_Map_File_Switch'Length + 2 .. Arg'Last));

         elsif Arg'Length >= 3 and then Arg (1 .. 3) = "-aP" then
            Forbidden_In_Package_Builder;

            if Arg'Length = 3 then
               Search_Project_Dir_Expected := True;

            else
               GPR.Env.Add_Directories
                 (Root_Environment.Project_Path, Arg (4 .. Arg'Last));
            end if;
         elsif Arg = "-a" then
            if not Dash_A_Warning_Issued then
               Put_Line (Dash_A_Warning);
               Dash_A_Warning_Issued := True;
            end if;

         elsif Arg = "-b" then
            Forbidden_With_Subst;
            Opt.Bind_Only  := True;

         elsif Arg = "-c" then
            Opt.Compile_Only := True;

            if Opt.Link_Only then
               Opt.Bind_Only  := True;
            end if;

         elsif Arg = "-C" then
            --  This switch is only for upward compatibility

            null;

         elsif Arg = "-d" then
            Opt.Display_Compilation_Progress := True;

         elsif Arg'Length = 3 and then Arg (2) = 'd' then
            if Arg (3) in '1' .. '9'
              or else Arg (3) in 'a' .. 'z'
              or else Arg (3) in 'A' .. 'Z'
            then
               Set_Debug_Flag (Arg (3));

            else
               Fail_Program
                 (Project_Tree, "illegal debug switch " & Arg);
            end if;

         elsif Arg'Length > 3 and then Arg (1 .. 3) = "-eI" then
            Forbidden_With_Subst;
            Forbidden_In_Package_Builder;

            begin
               Main_Index := Int'Value (Arg (4 .. Arg'Last));

            exception
               when Constraint_Error =>
                  Fail_Program (Project_Tree, "invalid switch " & Arg);
            end;

         elsif Arg = "-eL" then
            Forbidden_In_Package_Builder;
            Opt.Follow_Links_For_Files := True;
            Opt.Follow_Links_For_Dirs  := True;

         elsif Arg = "-eS" then
            Forbidden_In_Package_Builder;

            --  Accept switch for compatibility with gnatmake

         elsif Arg = "-f" then
            Opt.Force_Compilations := True;

            if Command_Line then
               Register_Command_Line_Option (Force_Compilations_Option);
            end if;

         elsif Arg = "-F" then
            Forbidden_In_Package_Builder;
            Opt.Full_Path_Name_For_Brief_Errors := True;

         elsif Arg = "-h" then
            Forbidden_In_Package_Builder;

         elsif Arg'Length > 2 and then Arg (2) = 'j' then
            declare
               Max_Proc : Natural := 0;
            begin
               for J in 3 .. Arg'Length loop
                  if Arg (J) in '0' .. '9' then
                     Max_Proc := (Max_Proc * 10) +
                       Character'Pos (Arg (J)) -
                       Character'Pos ('0');

                  else
                     Processed := False;
                  end if;
               end loop;

               if Processed then
                  if Max_Proc = 0 then
                     Max_Proc := Natural (Number_Of_CPUs);
                  end if;

                  if Max_Proc = 0 then
                     Max_Proc := 1;
                  end if;

                  Opt.Maximum_Processes := Max_Proc;
               end if;
            end;

            if Processed and then Command_Line then
               Register_Command_Line_Option
                 (Maximum_Processes_Option, Opt.Maximum_Processes);
            end if;

         elsif Arg = "-k" then
            Opt.Keep_Going := True;

            if Command_Line then
               Register_Command_Line_Option (Keep_Going_Option);
            end if;

         elsif Arg = "-l" then
            Forbidden_With_Subst;
            Opt.Link_Only  := True;

            if Opt.Compile_Only then
               Opt.Bind_Only := True;
            end if;

         elsif Arg = "-m" then
            Forbidden_With_Subst;
            Opt.Minimal_Recompilation := True;

         elsif Arg = "-o" then
            Forbidden_In_Package_Builder;

            if Output_File_Name /= null then
               Fail_Program
                 (Project_Tree, "cannot specify several -o switches");

            else
               Output_File_Name_Expected := True;
            end if;

         elsif Arg = "-p" or else Arg = "--create-missing-dirs" then
            Forbidden_In_Package_Builder;
            Opt.Setup_Projects := True;

         elsif Arg'Length >= 2 and then Arg (2) = 'P' then
            Forbidden_In_Package_Builder;

            if Project_File_Name /= null then
               Fail_Program
                 (Project_Tree,
                  "cannot have several project files specified");

            elsif Arg'Length = 2 then
               Project_File_Name_Expected := True;

            else
               Project_File_Name := new String'(Arg (3 .. Arg'Last));
            end if;

         elsif Arg = "-q" then
            Opt.Quiet_Output    := True;
            Opt.Verbose_Mode    := False;
            Opt.Verbosity_Level := None;

            if Command_Line then
               Register_Command_Line_Option (Quiet_Output_Option);
            end if;

         elsif Arg = "-r" then
            Forbidden_In_Package_Builder;
            Recursive := True;

         elsif Arg = "-R" then
            Forbidden_With_Subst;
            Opt.Run_Path_Option := False;

         elsif Arg = "-s" then
            Opt.Check_Switches := True;

            if Command_Line then
               Register_Command_Line_Option (Check_Switches_Option);
            end if;

         elsif Arg = "-u" then
            Forbidden_In_Package_Builder;
            Unique_Compile := True;

         elsif Arg = "-U" then
            Forbidden_In_Package_Builder;
            Unique_Compile_All_Projects := True;
            Unique_Compile := True;

         elsif Arg = "-v" or else Arg = "-vl" then
            Opt.Verbose_Mode    := True;
            Opt.Verbosity_Level := Opt.Low;
            Opt.Quiet_Output    := False;

            if Command_Line then
               Register_Command_Line_Option (Verbose_Low_Mode_Option);
            end if;

         elsif Arg = "-vm" then
            Opt.Verbose_Mode    := True;
            Opt.Verbosity_Level := Opt.Medium;
            Opt.Quiet_Output    := False;

            if Command_Line then
               Register_Command_Line_Option (Verbose_Medium_Mode_Option);
            end if;

         elsif Arg = "-vh" then
            Opt.Verbose_Mode    := True;
            Opt.Verbosity_Level := Opt.High;
            Opt.Quiet_Output    := False;

            if Command_Line then
               Register_Command_Line_Option (Verbose_High_Mode_Option);
            end if;

         elsif Arg'Length >= 3 and then Arg (1 .. 3) = "-vP" then
            Forbidden_In_Package_Builder;

            if Arg'Length = 4 and then  Arg (4) in '0' .. '2' then
               case Arg (4) is
               when '0' =>
                  Current_Verbosity := GPR.Default;
               when '1' =>
                  Current_Verbosity := GPR.Medium;
               when '2' =>
                  Current_Verbosity := GPR.High;
               when others =>
                  null;
               end case;

            else
               Fail_Program
                 (Project_Tree,
                  "invalid verbosity level " & Arg (4 .. Arg'Last));
            end if;

         elsif Arg = "-we" then
            Opt.Warning_Mode := Opt.Treat_As_Error;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Treat_As_Error);
            end if;

         elsif Arg = "-wn" then
            Opt.Warning_Mode := Opt.Normal;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Normal);
            end if;

         elsif Arg = "-ws" then
            Opt.Warning_Mode  := Opt.Suppress;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Suppress);
            end if;

         elsif Arg = "-x" then
            Opt.Use_Include_Path_File := True;

         elsif Arg = "-z" then
            Opt.No_Main_Subprogram := True;

         elsif Arg'Length >= 3
           and then Arg (2) = 'X'
           and then Is_External_Assignment (Root_Environment, Arg)
         then
            Forbidden_In_Package_Builder;

            --  Is_External_Assignment has side effects when it returns True

            null;

         elsif (Language = No_Name or else Language = Name_Ada)
           and then (not Command_Line)
           and then Arg = "-x"
         then
            --  For compatibility with gnatmake, ignore -x if found in the
            --  Builder switches.

            null;

         elsif (Language = No_Name or else Language = Name_Ada)
            and then not Subst_Switch_Present
            and then
             (Arg = "-fstack-check"
              or else Arg = "-fno-inline"
              or else
                (Arg'Length >= 2
                 and then (Arg (2) = 'O' or else Arg (2) = 'g')))
         then
            --  For compatibility with gnatmake, use switch to compile Ada
            --  code. We don't do this if the --compiler-pkg-subst switch was
            --  given, because the tool won't understand normal compiler
            --  options.

            if Command_Line then
               Current_Comp_Option_Table :=
                 Compiling_Options_HTable.Get (Name_Ada);

               if Current_Comp_Option_Table = No_Comp_Option_Table then
                  Current_Comp_Option_Table := new Compiling_Options.Instance;
                  Compiling_Options_HTable.Set
                    (Name_Ada, Current_Comp_Option_Table);
                  Compiling_Options.Init (Current_Comp_Option_Table.all);
               end if;

            else
               Current_Builder_Comp_Option_Table :=
                 Builder_Compiling_Options_HTable.Get (Name_Ada);

               if Current_Builder_Comp_Option_Table =
                  No_Builder_Comp_Option_Table
               then
                  Current_Builder_Comp_Option_Table :=
                    new Builder_Compiling_Options.Instance;
                  Builder_Compiling_Options_HTable.Set
                    (Name_Ada, Current_Builder_Comp_Option_Table);
                  Builder_Compiling_Options.Init
                    (Current_Builder_Comp_Option_Table.all);
               end if;
            end if;

            Current_Processor := Compiler;
            Add_Option (Arg, Command_Line);
            Current_Processor := None;

         elsif (Language = No_Name or else Language = Name_Ada)
            and then
             (Arg = "-nostdlib" or else Arg = "-nostdinc")
         then
            --  For compatibility with gnatmake, use switch to bind Ada code
            --  code and for -nostdlib to link.

            Current_Bind_Option_Table :=
              Binder_Options_HTable.Get (Name_Ada);

            if Current_Bind_Option_Table = No_Bind_Option_Table then
               Current_Bind_Option_Table := new Binder_Options.Instance;
               Binder_Options_HTable.Set
                 (Name_Ada, Current_Bind_Option_Table);
               Binder_Options.Init (Current_Bind_Option_Table.all);
            end if;

            Current_Processor := Binder;
            Add_Option (Arg, Command_Line);

            --  For -nostdlib, use the switch to link too

            if Arg = "-nostdlib" then
               Current_Processor := Linker;
               Add_Option (Arg, Command_Line);
            end if;

            Current_Processor := None;

         else
            Processed := False;
         end if;

      elsif Command_Line then
         --  The file name of a main or a project file

         declare
            File_Name : String := Arg;

         begin
            Canonical_Case_File_Name (File_Name);

            if File_Name'Length > Project_File_Extension'Length
              and then File_Name
                (File_Name'Last - Project_File_Extension'Length + 1
                 .. File_Name'Last) = Project_File_Extension
            then
               if Project_File_Name /= null then
                  Fail_Program
                    (Project_Tree,
                     "cannot have several project files specified");

               else
                  Project_File_Name := new String'(File_Name);
               end if;

            else
               --  Not a project file, then it is a main

               Mains.Add_Main (Arg);
               Always_Compile := True;
               Main_On_Command_Line := True;
            end if;
         end;

      else
         Processed := False;
      end if;

      if not Processed then
         if Command_Line then
            Fail_Program
              (Project_Tree,
               "illegal option """ & Arg & """ on the command line");

         else
            --  If --compiler-pkg-subst was given, we don't want to pass
            --  builder switches down to the "compiler", which probably won't
            --  understand them, because it's likely some tool like gnatpp.

            if Subst_Switch_Present then
               null;

            --  If we have a switch and there is a Builder Switches language
            --  set, pass this switch to the compiler of the language.

            elsif Arg (1) = '-' and then Builder_Switches_Lang /= No_Name then
               Current_Builder_Comp_Option_Table :=
                 Builder_Compiling_Options_HTable.Get (Builder_Switches_Lang);

               if Current_Builder_Comp_Option_Table =
                 No_Builder_Comp_Option_Table
               then
                  Current_Builder_Comp_Option_Table :=
                    new Builder_Compiling_Options.Instance;
                  Builder_Compiling_Options_HTable.Set
                    (Builder_Switches_Lang, Current_Builder_Comp_Option_Table);
                  Builder_Compiling_Options.Init
                    (Current_Builder_Comp_Option_Table.all);
               end if;

               Current_Processor := Compiler;
               Add_Option (Arg, False);
               Current_Processor := None;

            else
               Success := False;
            end if;
         end if;
      end if;
   end Scan_Arg;

   ------------------------
   -- Sigint_Intercepted --
   ------------------------

   procedure Sigint_Intercepted is
   begin
      Put_Line ("*** Interrupted ***");
      Delete_All_Temp_Files (Project_Tree.Shared);

      if Distributed_Mode then
         Compilation.Slave.Unregister_Remote_Slaves (From_Signal => True);
      end if;

      OS_Exit (1);
   end Sigint_Intercepted;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      procedure Check_Version_And_Help is new
        Check_Version_And_Help_G (Usage);
   begin
      --  Do some necessary package initializations

      Snames.Initialize;

      Set_Program_Name ("gprbuild");

      Set_Default_Verbosity;

      GPR.Tree.Initialize (Root_Environment, Gprbuild_Flags);
      GPR.Tree.Initialize (Project_Node_Tree);

      GPR.Initialize (Project_Tree);
      Mains.Delete;

      --  Get the name id for "-L";

      Name_Len := 0;
      Add_Str_To_Name_Buffer ("-L");
      Dash_L := Name_Find;

      --  Get the command line arguments, starting with --version and --help

      Check_Version_And_Help
        ("GPRBUILD",
         "2004",
         Version_String => Gpr_Version_String);

      --  Check for switch --dumpmachine and, if found, output the normalized
      --  hostname and exit.

      for Arg in 1 .. Argument_Count loop
         if Argument (Arg) = Dumpmachine then
            Gpr_Util.Knowledge.Parse_Knowledge_Base (Project_Tree);
            Put_Line (Gpr_Util.Knowledge.Normalized_Hostname);
            OS_Exit (0);
         end if;
      end loop;

      --  Check for switch -h an, if found, display usage and exit

      for Arg in 1 .. Argument_Count loop
         if Argument (Arg) = "-h" then
            Usage;
            OS_Exit (0);
         end if;
      end loop;

      --  Now process the other options

      Autoconfiguration := True;

      declare
         Do_Not_Care : Boolean;

      begin
         for Next_Arg in 1 .. Argument_Count loop
            declare
               Arg : constant String := Argument (Next_Arg);
            begin
               if (Arg'Length >= Compiler_Subst_Option'Length
                     and then
                   Arg (1 .. Compiler_Subst_Option'Length) =
                     Compiler_Subst_Option)
                 or else
                 (Arg'Length >= Compiler_Pkg_Subst_Option'Length
                    and then
                  Arg (1 .. Compiler_Pkg_Subst_Option'Length) =
                    Compiler_Pkg_Subst_Option)
               then
                  Subst_Switch_Present := True;
               end if;
            end;
         end loop;

         Scan_Args : for Next_Arg in 1 .. Argument_Count loop
            Scan_Arg
              (Argument (Next_Arg),
               Command_Line => True,
               Language     => No_Name,
               Success      => Do_Not_Care);
         end loop Scan_Args;
      end;

      if CodePeer_Mode then
         if Languages_Are_Restricted then
            Remove_All_Restricted_Languages;
         end if;

         Add_Restricted_Language ("ada");

         Opt.Link_Only := False;

         if not Opt.Compile_Only and not Opt.Bind_Only then
            Opt.Compile_Only := True;
            Opt.Bind_Only    := True;
         end if;

      elsif Languages_Are_Restricted then
         Opt.Compile_Only := True;
         Opt.Bind_Only    := False;
         Opt.Link_Only    := False;
      end if;

      Mains.Set_Multi_Unit_Index (Project_Tree, Main_Index);

      Current_Processor := None;

      GPR.Env.Initialize_Default_Project_Path
        (Root_Environment.Project_Path, Target_Name => "-");

      --  If --display-paths was specified, display the config and the user
      --  project paths and exit.

      if Display_Paths then
         Put ('.');

         declare
            Prefix_Path : constant String := Executable_Prefix_Path;
            P           : String_Access;

         begin
            if Prefix_Path'Length /= 0 then
               Put (Path_Separator);
               Put (Prefix_Path);
               Put ("share");
               Put (Directory_Separator);
               Put ("gpr");
            end if;

            New_Line;

            GPR.Env.Get_Path (Root_Environment.Project_Path, Path => P);
            Put_Line (P.all);
            Exit_Program (E_Success);
         end;
      end if;

      if Opt.Verbosity_Level > Opt.Low then
         Copyright;
      end if;

      --  Fail if command line ended with "-P"

      if Project_File_Name_Expected then
         Fail_Program
           (Project_Tree, "project file name missing after -P");

         --  Or if it ended with "-o"

      elsif Output_File_Name_Expected then
         Fail_Program
           (Project_Tree, "output file name missing after -o");

         --  Or if it ended with "-aP"

      elsif Search_Project_Dir_Expected then
         Fail_Program
           (Project_Tree, "directory name missing after -aP");

      elsif Db_Directory_Expected then
         Fail_Program
           (Project_Tree, "directory name missing after --db");

      elsif Slave_Env /= null and then not Distributed_Mode then
         Fail_Program
           (Project_Tree, "cannot use --slave-env in non distributed mode");
      end if;

      if Load_Standard_Base then
         --  We need to parse the knowledge base so that we are able to
         --  normalize the target names. Unfortunately, if we have to spawn
         --  gprconfig, it will also have to parse that knowledge base on
         --  its own.
         Parse_Knowledge_Base (Project_Tree);
      end if;

      --  If no project file is specified, look for a default

      if Project_File_Name = null then
         Look_For_Default_Project;

      else
         No_Project_File_Found := False;
      end if;

      if Project_File_Name = null then
         Try_Help;
         Fail_Program
           (Project_Tree,
            "no project file specified and no default project file");
      end if;

      --  Check consistency of out-of-tree build options.

      if Root_Dir /= null and then Build_Tree_Dir = null then
         Fail_Program
           (Project_Tree,
            "cannot use --root-dir without --relocate-build-tree option");
      end if;

      --  Set default Root_Dir

      if Build_Tree_Dir /= null and then Root_Dir = null then
         Root_Dir := new String'
           (Ada.Directories.Containing_Directory
              (Normalize_Pathname (Project_File_Name.all))
            & Dir_Separator);
      end if;
   end Initialize;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if not Usage_Output then
         Usage_Output := True;

         Put ("Usage: ");
         Put ("gprbuild [-P<proj>] [<proj>.gpr] [opts] [name]");
         New_Line;
         Put ("    {[-cargs opts] [-cargs:lang opts] [-largs opts]" &
                    " [-gargs opts]}");
         New_Line;
         New_Line;
         Put ("  name is zero or more file names");
         New_Line;
         New_Line;

         --  GPRBUILD switches

         Put ("gprbuild switches:");
         New_Line;

         Display_Usage_Version_And_Help;

         --  Line for --distributed

         Put ("  --distributed=slave1[,slave2]");
         New_Line;
         Put ("           Activate the remote/distributed compilations");
         New_Line;

         --  Line for --hash

         Put ("  --hash=string");
         New_Line;
         Put ("           Set an hash string to identified environment");
         New_Line;

         --  Line for --slave-env

         Put ("  --slave-env[=name]");
         New_Line;
         Put ("           Use a specific slave's environment");
         New_Line;
         New_Line;

         --  Line for --complete-output

         Put ("  --complete-output");
         New_Line;
         Put ("           Display all previous errors and warnings");
         New_Line;

         --  Line for --no-complete-output

         Put ("  --no-complete-output, -n");
         New_Line;
         Put ("           Do not store compilation outputs in files");
         New_Line;
         New_Line;

         --  Line for Config_Project_Option

         Put ("  ");
         Put (Config_Project_Option);
         Put ("file.cgpr");
         New_Line;
         Put ("           Specify the main config project file name");
         New_Line;

         --  Line for Autoconf_Project_Option

         Put ("  ");
         Put (Autoconf_Project_Option);
         Put ("file.cgpr");
         New_Line;
         Put
           ("           Specify/create the main config project file name");
         New_Line;

         --  Line for Target_Project_Option

         Put ("  ");
         Put (Target_Project_Option);
         Put ("targetname");
         New_Line;
         Put
           ("           Specify a target for cross platforms");
         New_Line;

         --  Line for --db

         Put ("  --db dir Parse dir as an additional knowledge base");
         New_Line;

         --  Line for --db-

         Put ("  --db-    Do not load the standard knowledge base");
         New_Line;

         --  Line for --relocate-build-tree=

         Put ("  --relocate-build-tree[=dir]");
         New_Line;
         Put ("           Root obj/lib/exec dirs are current-directory" &
                    " or dir");
         New_Line;

         --  Line for --root-dir=

         Put ("  --root-dir=dir");
         New_Line;
         Put ("           Root directory of obj/lib/exec to relocate");
         New_Line;

         --  Line for --subdirs=

         Put ("  --subdirs=dir");
         New_Line;
         Put ("           Real obj/lib/exec dirs are subdirs");
         New_Line;

         --  Line for --single-compile-per-obj-dir

         Put ("  ");
         Put (Single_Compile_Per_Obj_Dir_Switch);
         New_Line;
         Put
           ("           No simultaneous compilations for the same obj dir");
         New_Line;

         --  Line for --build-script=

         Put ("  ");
         Put (Build_Script_Option);
         Put_Line ("script_file");
         Put ("           Create build script script_file");
         New_Line;

         Put ("  ");
         Put (No_Indirect_Imports_Switch);
         New_Line;
         Put
           ("           Sources can import only from directly imported " &
            "projects");
         New_Line;

         Put ("  ");
         Put (Indirect_Imports_Switch);
         New_Line;
         Put
           ("           Sources can import from directly and indirectly " &
            "imported projects");
         New_Line;

         Put ("  --RTS=<runtime>");
         New_Line;
         Put ("           Use runtime <runtime> for language Ada");
         New_Line;

         Put ("  --RTS:<lang>=<runtime>");
         New_Line;
         Put ("           Use runtime <runtime> for language <lang>");
         New_Line;

         Put ("  ");
         Put (Gpr_Build_Util.Unchecked_Shared_Lib_Imports);
         New_Line;
         Put ("           Shared lib projects may import any project");
         New_Line;

         Put ("  ");
         Put (No_Object_Check_Switch);
         New_Line;
         Put ("           Do not check object files");
         New_Line;

         Put ("  --no-sal-binding");
         New_Line;
         Put ("           Reuse binder files when linking SALs");
         New_Line;

         Put ("  ");
         Put (Restricted_To_Languages_Option);
         Put ("<list of languages>");
         New_Line;
         Put ("           Restrict the languages of the sources");
         New_Line;
         New_Line;

         Put ("  ");
         Put (Create_Map_File_Switch);
         New_Line;
         Put ("           Create map file mainprog.map");
         New_Line;
         Put ("  ");
         Put (Create_Map_File_Switch);
         Put ("=mapfile");
         New_Line;
         Put ("           Create map file mapfile");
         New_Line;
         New_Line;

         --  Line for -aP

         Put ("  -aP dir  Add directory dir to project search path");
         New_Line;

         --  Line for -b

         Put ("  -b       Bind only");
         New_Line;

         --  Line for -c

         Put ("  -c       Compile only");
         New_Line;

         --  Line for -d

         Put ("  -d       Display compilation progress");
         New_Line;

         --  Line for -eInn

         Put ("  -eInn    Index of main unit in multi-unit source file");
         New_Line;

         --  Line for -eL

         Put ("  -eL      " &
                    "Follow symbolic links when processing project files");
         New_Line;

         --  Line for -eS

         Put ("  -eS      " &
                    "(no action, for compatibility with gnatmake only)");
         New_Line;

         --  Line for -f

         Put ("  -f       Force recompilations");
         New_Line;

         --  Line for -F

         Put
           ("  -F       Full project path name in brief error messages");
         New_Line;

         --  Line for -jnnn

         Put ("  -jnum    Use num processes to compile");
         New_Line;

         --  Line for -k

         Put ("  -k       Keep going after compilation errors");
         New_Line;

         --  Line for -l

         Put ("  -l       Link only");
         New_Line;

         --  Line for -m

         Put ("  -m       Minimum Ada recompilation");
         New_Line;

         --  Line for -o

         Put ("  -o name  Choose an alternate executable name");
         New_Line;

         --  Line for -p

         Put ("  -p       Create missing obj, lib and exec dirs");
         New_Line;

         --  Line for -P

         Put ("  -P proj  Use Project File proj");
         New_Line;

         --  Line for -q

         Put ("  -q       Be quiet/terse");
         New_Line;

         --  Line for -r

         Put ("  -r       Recursive (default except when using -c)");
         New_Line;

         --  Line for -R

         Put ("  -R       Do not use run path option");
         New_Line;

         --  Line for -s

         Put ("  -s       Recompile if compiler switches have changed");
         New_Line;

         --  Line for -u

         Put
           ("  -u       Unique compilation, only compile the given files");
         New_Line;

         --  Line for -U

         Put
           ("  -U       Unique compilation for all sources of all projects");
         New_Line;

         --  Line for -v

         Put ("  -v       Verbose output");
         New_Line;

         --  Line for -vl

         Put ("  -vl      Verbose output (low verbosity)");
         New_Line;

         --  Line for -vm

         Put ("  -vm      Verbose output (medium verbosity)");
         New_Line;

         --  Line for -vh

         Put ("  -vh      Verbose output (high verbosity)");
         New_Line;

         --  Line for -vPx

         Put ("  -vPx     Specify verbosity when parsing Project Files" &
                    " (x = 0/1/2)");
         New_Line;

         --  Line for -we

         Put ("  -we      Treat all warnings as errors");
         New_Line;

         --  Line for -wn

         Put ("  -wn      Treat warnings as warnings");
         New_Line;

         --  Line for -ws

         Put ("  -ws      Suppress all warnings");
         New_Line;

         --  Line for -x

         Put ("  -x       Always create include path file");
         New_Line;

         --  Line for -X

         Put ("  -Xnm=val Specify an external reference for " &
                    "Project Files");
         New_Line;
         New_Line;

         --  Line for -z

         Put ("  -z       No main subprogram (zero main)");
         New_Line;

         --  Line for --compiler-subst

         Put_Line ("  --compiler-subst=lang,tool    Specify alternate " &
                     "compiler");

         --  Line for --compiler-pkg-subst

         Put_Line ("  --compiler-pkg-subst=pkg    Specify alternate " &
                     "package");
         New_Line;

         --  Line for -dn

         Put ("  -dn      Do not delete temporary files");
         New_Line;
         New_Line;

         --  Line for -cargs

         Put_Line ("  -cargs opts    opts are passed to all compilers");

         --  Line for -cargs:lang

         Put_Line ("  -cargs:<lang> opts");
         Put_Line ("                 opts are passed to the compiler " &
                     "for language <lang> ");

         --  Line for -bargs

         Put_Line ("  -bargs opts    opts are passed to all binders");

         --  Line for -cargs:lang

         Put_Line ("  -bargs:<lang> opts");
         Put_Line ("                 opts are passed to the binder " &
                     "for language <lang> ");

         --  Line for -largs

         Put ("  -largs opts    opts are passed to the linker");
         New_Line;

         --  Line for -gargs

         Put ("  -gargs opts    opts directly interpreted by gprbuild");
         New_Line;

         --  Line for -margs

         Put ("  -margs opts    equivalent to -gargs opts");
         New_Line;

         New_Line;

         Put
           ("For compatibility with gnatmake, these switches are passed " &
            "to the Ada compiler:");
         New_Line;

         Put ("  -nostdlib");
         New_Line;

         Put ("  -nostdinc");
         New_Line;

         Put ("  -fstack-check");
         New_Line;

         Put ("  -fno-inline");
         New_Line;

         Put ("  -gxxx");
         New_Line;

         Put ("  -Oxx");
         New_Line;

         New_Line;
      end if;
   end Usage;

   User_Project_Node : Project_Node_Id;

   procedure Do_Compute_Builder_Switches is
      new Compute_Builder_Switches (Add_Global_Switches);

begin
   --  First initialize and read the command line arguments

   Initialize;

   --  And install Ctrl-C handler

   Install_Int_Handler (Sigint_Intercepted'Unrestricted_Access);

   --  Check command line arguments. These will be overridden when looking
   --  for the configuration file

   if Target_Name = null then
      Target_Name := new String'("");
   end if;

   if Config_Project_File_Name = null then
      Config_Project_File_Name := new String'("");

   elsif Autoconf_Specified then
      --  Check if path needs to be created

      declare
         Config_Path : constant String :=
                         Ada.Directories.Containing_Directory
                           (Config_Project_File_Name.all);
      begin
         if not Ada.Directories.Exists (Config_Path) then
            Ada.Directories.Create_Path (Config_Path);
         end if;
      end;
   end if;

   --  Then, parse the user's project and the configuration file. Apply the
   --  configuration file to the project so that its settings are
   --  automatically inherited by the project.
   --  If either the project or the configuration file contains errors, the
   --  following call with call Fail_Program and never return

   begin
      Main_Project := No_Project;
      Parse_Project_And_Apply_Config
        (Main_Project               => Main_Project,
         User_Project_Node          => User_Project_Node,
         Config_File_Name           => Config_Project_File_Name.all,
         Autoconf_Specified         => Autoconf_Specified,
         Project_File_Name          => Project_File_Name.all,
         Project_Tree               => Project_Tree,
         Env                        => Root_Environment,
         Project_Node_Tree          => Project_Node_Tree,
         Packages_To_Check          => Packages_To_Check,
         Allow_Automatic_Generation => Autoconfiguration,
         Automatically_Generated    => Delete_Autoconf_File,
         Config_File_Path           => Configuration_Project_Path,
         Target_Name                => Target_Name.all,
         Normalized_Hostname        => Normalized_Hostname,
         Implicit_Project           => No_Project_File_Found);
   exception
      when E : GPR.Conf.Invalid_Config =>
         Fail_Program (Project_Tree, Exception_Message (E));
   end;

   if Main_Project = No_Project then
      --  Don't flush messages in case of parsing error. This has already
      --  been taken care when parsing the tree. Otherwise, it results in
      --  the same message being displayed twice.

      Fail_Program
        (Project_Tree,
         """" & Project_File_Name.all & """ processing failed",
         Flush_Messages => User_Project_Node /= Empty_Project_Node);
   end if;

   if Configuration_Project_Path /= null then
      Free (Config_Project_File_Name);
      Config_Project_File_Name := new String'
        (Base_Name (Configuration_Project_Path.all));
   end if;

   if Total_Errors_Detected > 0 then
      GPR.Err.Finalize;
      Fail_Program
        (Project_Tree,
         "problems while getting the configuration",
         Flush_Messages => False);
   end if;

   --  Warn if there have been binder option specified on the command line
   --  and the main project is a Stand-Alone Library project.

   declare
      Options_Instance : constant Bind_Option_Table_Ref :=
        Binder_Options_HTable.Get (Name_Ada);
   begin
      if All_Language_Binder_Options.Last > 0 or else
        (Options_Instance /= No_Bind_Option_Table and then
         Binder_Options.Last (Options_Instance.all) > 0)
      then
         if Main_Project.Standalone_Library /= No then
            GPR.Err.Error_Msg
              ("?binding options on the command line are not taken " &
               "into account when the main project is a Stand-Alone " &
               "Library project",
               Main_Project.Location);
         end if;
      end if;
   end;

   Main_Project_Dir :=
     new String'(Get_Name_String (Main_Project.Directory.Display_Name));

   if Warnings_Detected > 0 then
      GPR.Err.Finalize;
      GPR.Err.Initialize;
   end if;

   --  Adjust switches for "c" target: never perform the link phase

   declare
      C_Target : Boolean := False;
      Variable : Variable_Value;
   begin
      if Target_Name.all = "c" then
         C_Target := True;
      else
         Variable := GPR.Util.Value_Of
           (Name_Target, Main_Project.Decl.Attributes, Project_Tree.Shared);

         if Variable /= Nil_Variable_Value
           and then Get_Name_String (Variable.Value) = "c"
         then
            C_Target := True;
         end if;
      end if;

      if C_Target then
         Opt.Link_Only := False;

         if not Opt.Compile_Only and not Opt.Bind_Only then
            Opt.Compile_Only := True;
            Opt.Bind_Only    := True;
         end if;
      end if;
   end;

   Compute_All_Imported_Projects (Main_Project, Project_Tree);

   if Main_Project.Qualifier = Aggregate_Library then
      if Main_On_Command_Line then
         Fail_Program
           (Project_Tree,
            "cannot specify a main program " &
            "on the command line for a library project file");
      end if;
   else
      if Mains.Number_Of_Mains (Project_Tree) = 0
        and then not Unique_Compile
      then
         --  Register the Main units from the projects.
         --  No need to waste time when we are going to compile all files
         --  anyway (Unique_Compile).
         Mains.Fill_From_Project (Main_Project, Project_Tree);
      end if;

      Mains.Complete_Mains
        (Root_Environment.Flags, Main_Project, Project_Tree, Unique_Compile);

      if not Unique_Compile
        and then Output_File_Name /= null
        and then Mains.Number_Of_Mains (null) > 1
      then
         Fail_Program
           (Project_Tree, "cannot specify -o when there are several mains");
      end if;
   end if;

   Do_Compute_Builder_Switches
     (Project_Tree     => Project_Tree,
      Env              => Root_Environment,
      Main_Project     => Main_Project);

   Queue.Initialize (Opt.One_Compilation_Per_Obj_Dir);

   Compute_Compilation_Phases
     (Project_Tree,
      Main_Project,
      Option_Unique_Compile => Unique_Compile,
      Option_Compile_Only   => Opt.Compile_Only,
      Option_Bind_Only      => Opt.Bind_Only,
      Option_Link_Only      => Opt.Link_Only);

   if Mains.Number_Of_Mains (Project_Tree) > 0
     and then Main_Project.Library
     and then Builder_Data (Project_Tree).Need_Binding
   then
      Fail_Program
        (Project_Tree,
         "cannot specify a main program " &
           "on the command line for a library project file");
   end if;

   Add_Mains_To_Queue;

   --  If no sources to compile, then there is nothing to do

   if Queue.Size = 0 then
      if not Opt.Quiet_Output
        and then not Main_Project.Externally_Built
      then
         Write_Program_Name;
         Write_Line ("no sources to compile");
      end if;

      Finish_Program (Project_Tree, E_Success);
   end if;

   Always_Compile :=
     Always_Compile
     and then Opt.Force_Compilations
     and then Unique_Compile
     and then not Unique_Compile_All_Projects;

   --  Reprocess recorded command line options that have priority over
   --  those in the main project file.

   Options.Process_Command_Line_Options;

   --  If a build script is declared, try to create the file. Fail if the file
   --  cannot be created.

   if Build_Script_Name /= null then
      begin
         Create (Build_Script_File, Out_File, Build_Script_Name.all);

      exception
         when others =>
            Fail_Program
              (null,
               "build script """ &
               Build_Script_Name.all &
               """ could not be created");
      end;
   end if;

   if Debug.Debug_Flag_M then
      Put_Line ("Maximum number of simultaneous compilations =" &
                    Opt.Maximum_Processes'Img);
   end if;

   --  Warn if --create-map-file is not supported

   if Map_File /= null
     and then Main_Project.Config.Map_File_Option = No_Name
   then
      Put ("warning: option ");
      Put (Create_Map_File_Switch);
      Put (" is not supported in this configuration");
      New_Line;
   end if;

   --  Set slave-env

   if Slave_Env = null and then Distributed_Mode then
      Slave_Env :=
        new String'(Compute_Slave_Env (Project_Tree, Slave_Env_Auto));

      if Slave_Env_Auto and not Opt.Quiet_Output then
         Put ("slave environment is ");
         Put (Slave_Env.all);
         New_Line;
      end if;
   end if;

   Compile.Run;

   --  If the build script file is opened, close it, so that it can be reopened
   --  by gprlib and gprbind.

   if Is_Open (Build_Script_File) then
      Close (Build_Script_File);
      Opt.Maximum_Processes := 1;
   end if;

   Post_Compile.Run;
   Link.Run;

   if Warnings_Detected /= 0 then
      GPR.Err.Finalize;
   end if;

   Finish_Program (Project_Tree, Exit_Code);

exception
   when C : Constraint_Error =>
      if Distributed_Mode then
         Compilation.Slave.Unregister_Remote_Slaves (From_Signal => True);
      end if;

      Fail_Program (Project_Tree, Exception_Message (C));

   when E : others =>
      Fail_Program (Project_Tree, Exception_Information (E));
end Gprbuild.Main;
