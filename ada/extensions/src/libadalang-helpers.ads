------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C)      2019, AdaCore                     --
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
with GNATCOLL.Projects;

with Libadalang.Analysis; use Libadalang.Analysis;

package Libadalang.Helpers is

   package Unit_Vectors is new
     Ada.Containers.Vectors (Positive, Analysis_Unit);

   Abort_App_Exception : exception;
   procedure Abort_App (Message : String := "") with No_Return;
   --  If provided, print Message to the standard error output and raise an
   --  Abort_App_Exception.

   type App_Context is record
      Project : GNATCOLL.Projects.Project_Tree_Access;
      --  If the App loaded a context, reference to it, null otherwise
   end record;

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

   type App_Job_Context_Array is array (Job_ID range <>) of App_Job_Context;

   --  This package is a convenient and easy way to create an application based
   --  on Libadalang, with out of the box facilities such as:
   --
   --  * Automatic handling of project files, including an option to process
   --    all the files of the project, and handling of scenario variables.
   --
   --  * Automatic command line option parser, with descriptive help.
   --
   --  As a user, all you have to do is to provide a ``Process_Unit``
   --  procedure, that will process one unit out of the set of units
   --  to process.
   generic
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
      --      Job_Tear_Down.
      --
      --    * Job 2 calls Job_Setup, then several Process_Units, then
      --      Job_Tear_Down.
      --
      --    * Job 3 calls ...
      --
      --  Finally, once all jobs are done, the main task calls App_Tear_Down.

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

      with procedure Job_Tear_Down (Context : App_Job_Context) is null;
      --  This procedure will be called once after all units have been parsed.
      --  Note it will be called once per job.

      with procedure App_Tear_Down
        (Context : App_Context; Jobs : App_Job_Context_Array) is null;
      --  This procedure is called once all jobs are done

   package App is

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

         package Scenario_Vars is new Parse_Option_List
           (Parser, Short => "-X", Long => "--scenario-variable",
            Arg_Type      => Unbounded_String,
            Accumulate    => True,
            Help          => "Scenario variables to pass to the project file");

         package Project_File is new Parse_Option
           (Parser, "-P", "--project",
            Arg_Type    => Unbounded_String,
            Default_Val => Null_Unbounded_String,
            Help        => "Project file to use");

         package Jobs is new Parse_Option
           (Parser, "-j", "--jobs",
            Arg_Type    => Natural,
            Default_Val => 1,
            Help        => "Number of parallel jobs to use",
            Enabled     => Enable_Parallelism);

         package No_Traceback is new Parse_Flag
           (Parser, Long => "--no-traceback",
            Help         => "Do not display traceback for exceptions");

         package Sym_Traceback is new Parse_Flag
           (Parser, Long => "--symbolic-traceback",
            Help         => "Show symbolic tracebacks for exceptions");

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
      --
      --  If one callback raises an Abort_App_Error exception, this catches it
      --  (i.e. the exception is not propagated to callers), set the process
      --  exit status to Failure (see Ada.Command_Line) and returns.

      procedure Dump_Exception (E : Ada.Exceptions.Exception_Occurrence);
      --  Dump the exception E, honoring the Args.No_Traceback flag (i.e.
      --  don't show tracebacks when asked not to).
   end App;

end Libadalang.Helpers;
