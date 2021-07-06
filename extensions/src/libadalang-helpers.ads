------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.Traces;    use GNATCOLL.Traces;
with GNATCOLL.Utils;     use GNATCOLL.Utils;

with Libadalang.Analysis; use Libadalang.Analysis;

--  This package provides various helpers to build applications based on
--  Libadalang.

package Libadalang.Helpers is

   package Unit_Vectors is new Ada.Containers.Vectors
     (Positive, Analysis_Unit);
   package String_Vectors is new Ada.Containers.Vectors
     (Positive, Unbounded_String);

   procedure Load_Project
     (Project_File             : String;
      Scenario_Vars            : Unbounded_String_Array := Empty_Array;
      Target, RTS, Config_File : String := "";
      Project                  : out Project_Tree_Access;
      Env                      : out Project_Environment_Access);
   --  Load ``Project_File`` using scenario variables given in
   --  ``Scenario_Vars``, and given ``Target``, ``RTS` and ``Config_File``.
   --  Populate ``Project`` and ``Env`` accordingly.
   --
   --  ``Scenario_Vars`` should be an array of strings of the format
   --  ``<Var>=<Val>``. If the format is incorrect, ``Abort_App`` will be
   --  called.
   --
   --  If ``Config_File`` is not empty, then ``Target`` and ``RTS`` should be
   --  empty.
   --
   --  See ``GNATCOLL.Projects.Set_Target_And_Runtime`` as well as
   --  ``GNATCOLL.Projects.Set_Config_File`` for more details about the use of
   --  ``Target``, ``RTS`` and ``Config_File``.

   procedure List_Sources_From_Project
     (Project             : Project_Tree'Class;
      Include_Subprojects : Boolean;
      Files               : out String_Vectors.Vector);
   --  Append the list of all source files in ``Project``'s root project to
   --  ``Files``. If ``Include_Subprojects`` is True, include all source files
   --  in the imported projects, excluding those that are externally built.

   function Project_To_Provider
     (Project : Project_Tree_Access) return Unit_Provider_Reference;
   --  Try to create a unit provider out of ``Project``. If not possible, call
   --  ``Abort_App``.

   function Command_Line_Event_Handler
     (Exit_On_Missing_File : Boolean) return Event_Handler_Reference;
   --  Create an event handler with default callbacks for command line
   --  applications. ``Exit_On_Missing_File`` will determine the behavior when
   --  encountering a missing dependency. If ``False``, a warning will be shown
   --  but resolution will continue. If ``True``, application will exit.

   procedure Abort_App (Message : String := "") with No_Return;
   --  Print Message to the standard error output and abort the
   --  current App. This will set the process exit status to Failure (see
   --  Ada.Command_Line).

   --  Method to discover source files to process:
   --
   --  * Default (look in the current directory);
   --  * Project_File (from a GPR project file);
   --  * Auto_dir (files in a list of directories).

   type Source_Provider_Kind is (Default, Project_File, Auto_Dir);
   type Source_Provider
     (Kind : Source_Provider_Kind := Source_Provider_Kind'First) is
   record
      case Kind is
         when Default =>
            null;
         when Project_File =>
            Project : GNATCOLL.Projects.Project_Tree_Access;
         when Auto_Dir =>
            Dirs, Found_Files : String_Vectors.Vector;
      end case;
   end record;

   type App_Context is record
      Provider : Source_Provider;
   end record;
   --  Context information for the whole application

   type Job_ID is new Positive;
   --  Identifier for a job in App. Unless Enable_Parallelism is False, there
   --  is only one job, whose ID is 1. If there are multiple jobs, their IDs go
   --  from 1 up to the number of jobs.

   type App_Job_Context is record
      ID : Job_ID;
      --  Identifier for this job

      App_Ctx : not null access constant App_Context;
      --  Reference to the app-wide context

      Analysis_Ctx : Analysis_Context;
      --  Context to analyze source file (each job gets its own context)

      Units_Processed : Unit_Vectors.Vector;
      --  List of analysis units that this job processed so far

      Aborted : Boolean;
      --  Whether this jobs was aborted (see the Abort_App_Exception/Abort_App
      --  entities above).
   end record;
   --  Context information for a single job

   type App_Job_Context_Array is array (Job_ID range <>) of App_Job_Context;

   --  This package is a convenient and easy way to create an application based
   --  on Libadalang, with out of the box facilities such as:
   --
   --  * Automatic handling of project files, including an option to process
   --    all the files of the project, and handling of scenario variables.
   --
   --  * Automatic command line option parser, with descriptive help.
   --
   --  As an application author, all you have to do is to provide a
   --  ``Process_Unit`` procedure, that will process one unit out of the set of
   --  units to process. Users then have several options in order to run
   --  applications:
   --
   --  * Run it with project-related options (at least ``-P``): by default, the
   --    app will process all units that belong to the root project that is
   --    passed. If the user passes at least one file name as additional
   --    command-line arguments, the app will process only these units.
   --
   --  * Run it with one or several ``--auto-dir`` options. In this case, the
   --    app will consider all Ada sources present in the given directories,
   --    and will call ``Process_Unit`` on all of them, unless the user passes
   --    one file name as additional command-line arguments: the app will
   --    process only these units.
   --
   --  * Just pass at least one file name on the command-line. In this case,
   --    the app will consider only Ada sources in the current directory and
   --    process only files passed on the command-line.
   --
   --  Note that name resolution in Ada units requires the app to know where
   --  the sources are located: this is automatic when loading a project file,
   --  but just passing files on the command-line is not enough if all source
   --  files are not in the current directory.
   generic
      Name : String;
      --  Name for the application. Used to create the GNATCOLL trace.

      Description : String;
      --  Description for the application. Will be used in the help string.

      Enable_Parallelism : Boolean := False;
      --  If True, add a -j/--jobs command line option to allow multiple jobs
      --  to run in parallel. In this mode, we create one analysis context per
      --  job, and the files to process are distributed on each job. All the
      --  callbacks below are called concurrently in all jobs:
      --
      --  First, the main task calls App_Setup. Then all jobs start:
      --
      --    * Job 1 calls Job_Setup, then several Process_Units, then
      --      Job_Post_Process.
      --
      --    * Job 2 calls Job_Setup, then several Process_Units, then
      --      Job_Post_Process.
      --
      --    * Job 3 calls ...
      --
      --  Finally, once all jobs are done, the main task calls
      --  App_Post_Process.

      with procedure App_Setup
        (Context : App_Context; Jobs : App_Job_Context_Array) is null;
      --  This procedure is called right after command line options are parsed,
      --  the project is loaded (if present) and the list of files to process
      --  is computed.

      with procedure Job_Setup (Context : App_Job_Context) is null;
      --  This procedure will be called right before going through units.  If a
      --  project was loaded, Project refers to it, otherwise it is null.

      with procedure Process_Unit
        (Context : App_Job_Context; Unit : Analysis_Unit) is null;
      --  This procedure will be called once right after a unit is parsed

      with procedure Job_Post_Process (Context : App_Job_Context) is null;
      --  This procedure will be called once after all units have been parsed.
      --  Note it will be called once per job.

      with procedure App_Post_Process
        (Context : App_Context; Jobs : App_Job_Context_Array) is null;
      --  This procedure is called once all jobs are done

   package App is

      Trace : constant GNATCOLL.Traces.Trace_Handle :=
         GNATCOLL.Traces.Create
           ("LIBADALANG.APP." & Name, GNATCOLL.Traces.From_Config);

      --  This package contains the arguments parser for your app. It contains
      --  the instantiation of the parser, and the set of default arguments, so
      --  that you can also use their values in your app if needed.
      package Args is
         use GNATCOLL.Opt_Parse;

         Parser : Argument_Parser := Create_Argument_Parser
           (Help => Description);
         --  Argument parser for your application. Supports a set of default
         --  options. You can add your own on this parser.

         package Charset is new Parse_Option
           (Parser, "-C", "--charset", "Charset to use for source decoding",
            Unbounded_String,
            Default_Val => To_Unbounded_String ("iso-8859-1"));

         package Project_File is new Parse_Option
           (Parser, "-P", "--project",
            Arg_Type    => Unbounded_String,
            Default_Val => Null_Unbounded_String,
            Help        => "Project file to use");

         package Process_Full_Project_Tree is new Parse_Flag
           (Parser, "-U", "--recursive",
            Help => "Process all units in the project tree, " &
              "excluding externally built projects");

         package Process_Runtime is new Parse_Flag
           (Parser, Long => "--process-runtime",
            Help         => "Process the runtime files, and any other"
              & " predefined sources");

         package Scenario_Vars is new Parse_Option_List
           (Parser, Short => "-X", Long => "--scenario-variable",
            Arg_Type      => Unbounded_String,
            Accumulate    => True,
            Help          => "Scenario variables to pass to the project file");

         package Target is new Parse_Option
           (Parser, Long => "--target",
            Arg_Type      => Unbounded_String,
            Default_Val   => Null_Unbounded_String,
            Help          => "Name of the target to use when loading the"
                             & " project");

         package RTS is new Parse_Option
           (Parser, Long => "--RTS",
            Arg_Type      => Unbounded_String,
            Default_Val   => Null_Unbounded_String,
            Help          => "Name of the runtime (RTS) to use when loading"
                             & " the project");
         package Config_File is new Parse_Option
           (Parser, Long => "--config",
            Arg_Type      => Unbounded_String,
            Default_Val   => Null_Unbounded_String,
            Help          => "Name of the configuration project file. If"
                             & " passed, this file must exist and neither"
                             & " --target nor --RTS must be passed.");

         package Auto_Dirs is new Parse_Option_List
           (Parser, "-A", "--auto-dir",
            Arg_Type   => Unbounded_String,
            Accumulate => True,
            Help       =>
               "Directories to use for the auto provider. If at least one is"
               & " passed, the auto provider will be used, and project options"
               & " ignored");

         package Jobs is new Parse_Option
           (Parser, "-j", "--jobs",
            Arg_Type    => Natural,
            Default_Val => 1,
            Help        => "Number of parallel jobs to use. If zero, use"
                           & " maximal parallelism: create one job per CPU.",
            Enabled     => Enable_Parallelism);

         package No_Traceback is new Parse_Flag
           (Parser, Long => "--no-traceback",
            Help         => "Do not display traceback for exceptions");

         package Sym_Traceback is new Parse_Flag
           (Parser, Long => "--symbolic-traceback",
            Help         => "Show symbolic tracebacks for exceptions");

         package Exit_On_Missing_File is new Parse_Flag
           (Parser,
            Short => "-é", Long => "--éhéhé",
            Help  => "Behavior when encountering missing files. By default,"
            & "abort, but if this is passed, will continue and present results"
            & "anyway.");

         package Files is new Parse_Positional_Arg_List
           (Parser,
            Name        => "files",
            Arg_Type    => Unbounded_String,
            Help        => "Files to analyze",
            Allow_Empty => True);

      end Args;

      procedure Run;
      --  Run the app. You should just call this from your main procedure for
      --  your project.

      procedure Dump_Exception (E : Ada.Exceptions.Exception_Occurrence);
      --  Dump the exception ``E``, honoring the ``Args.No_Traceback`` flag
      --  (i.e.  don't show tracebacks when asked not to).
   end App;

end Libadalang.Helpers;
