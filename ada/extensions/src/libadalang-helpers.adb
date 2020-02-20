------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2020, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Traceback.Symbolic;

with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.VFS;       use GNATCOLL.VFS;

with Libadalang.Auto_Provider;    use Libadalang.Auto_Provider;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

package body Libadalang.Helpers is

   Abort_App_Exception : exception;
   --  Exception used to abort the execution of an App in a user callback. See
   --  the Abort_App procedure.

   function "+" (S : String) return Unbounded_String
                 renames To_Unbounded_String;
   function "+" (S : Unbounded_String) return String renames To_String;

   procedure Print_Error (Message : String);
   --  Helper to print Message on the standard error

   package String_QI is new Ada.Containers.Synchronized_Queue_Interfaces
     (Unbounded_String);
   package String_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (String_QI);

   -----------------
   -- Print_Error --
   -----------------

   procedure Print_Error (Message : String) is
   begin
      --  If Message's last character is a newline, leave it out and let
      --  Put_Line append it. This avoids the additional line break that
      --  Text_IO would append later otherwise.

      if Message = "" then
         return;
      elsif Message (Message'Last) = ASCII.LF then
         Put_Line
           (Standard_Error, Message (Message'First .. Message'Last - 1));
      else
         Put_Line (Standard_Error, Message);
      end if;
   end Print_Error;

   ---------------
   -- Abort_App --
   ---------------

   procedure Abort_App (Message : String := "") is
   begin
      if Message /= "" then
         Put_Line (Standard_Error, Message);
      end if;
      raise Abort_App_Exception;
   end Abort_App;

   package body App is

      --  The following protected object is used for a job to signal to the
      --  other jobs that it has aborted. In this case, the other jobs must
      --  finish processing their current analysis unit and stop there.

      protected Abortion is
         procedure Signal_Abortion;
         function Abort_Signaled return Boolean;
      private
         Abort_Signaled_State : Boolean := False;
      end Abortion;

      procedure Load_Project
        (Project_File             : String;
         Scenario_Vars            : Args.Scenario_Vars.Result_Array;
         Target, RTS, Config_File : String;
         Project                  : out Project_Tree_Access;
         Env                      : out Project_Environment_Access);
      --  Load Project_File using the given scenario variables

      function Project_To_Provider
        (Project : Project_Tree_Access) return Unit_Provider_Reference;
      --  Try to create a unit provider out of Project. If not possible, call
      --  Abort_App.

      procedure List_Sources_From_Project
        (Project : Project_Tree'Class; Files : out String_Vectors.Vector);
      --  Append the list of all source files in Project's root project to
      --  Files.

      function Files_From_Args
        (Files : out String_Vectors.Vector) return Boolean;
      --  If source files are passed on the command line, append them to Files
      --  and return True. Do nothing and return False otherwise.

      protected body Abortion is
         procedure Signal_Abortion is
         begin
            Abort_Signaled_State := True;
         end Signal_Abortion;

         function Abort_Signaled return Boolean is
         begin
            return Abort_Signaled_State;
         end Abort_Signaled;
      end Abortion;

      --------------------
      -- Dump_Exception --
      --------------------

      procedure Dump_Exception (E : Ada.Exceptions.Exception_Occurrence) is
      begin
         if Args.No_Traceback.Get then
            --  Do not use Exception_Information nor Exception_Message. The
            --  former includes tracebacks and the latter includes line
            --  numbers in Libadalang: both are bad for testcase output
            --  consistency.
            Put_Line ("> " & Ada.Exceptions.Exception_Name (E));
            New_Line;

         elsif Args.Sym_Traceback.Get then
            Put_Line (Ada.Exceptions.Exception_Message (E));
            Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

         else
            Put_Line ("> " & Ada.Exceptions.Exception_Information (E));
            New_Line;
         end if;
      end Dump_Exception;

      ------------------
      -- Load_Project --
      ------------------

      procedure Load_Project
        (Project_File             : String;
         Scenario_Vars            : Args.Scenario_Vars.Result_Array;
         Target, RTS, Config_File : String;
         Project                  : out Project_Tree_Access;
         Env                      : out Project_Environment_Access)
      is
         procedure Cleanup;
         --  Cleanup helpers for error handling

         -------------
         -- Cleanup --
         -------------

         procedure Cleanup is
         begin
            Free (Project);
            Free (Env);
         end Cleanup;
      begin
         Trace.Trace ("Loading project " & Project_File);
         Project := new Project_Tree;
         Initialize (Env);

         --  Set scenario variables
         for Assoc of Scenario_Vars loop
            declare
               A        : constant String := +Assoc;
               Eq_Index : Natural := A'First;
            begin
               while Eq_Index <= A'Last
                 and then A (Eq_Index) /= '=' loop
                  Eq_Index := Eq_Index + 1;
               end loop;
               if Eq_Index not in A'Range then
                  Cleanup;
                  Abort_App ("Invalid scenario variable: -X" & A);
               end if;
               Change_Environment
                 (Env.all,
                  A (A'First .. Eq_Index - 1),
                  A (Eq_Index + 1 .. A'Last));
            end;
         end loop;

         --  Set the target/runtime or use the config file
         if Config_File = "" then
            Env.Set_Target_And_Runtime (Target, RTS);
         elsif Target /= "" or else RTS /= "" then
            Cleanup;
            Abort_App ("--config not allowed if --target or --RTS are passed");
         else
            Env.Set_Config_File (Create (+Config_File));
         end if;

         --  Load the project tree, and beware of loading errors. Wrap
         --  the project in a unit provider.
         begin
            Project.Load
              (Root_Project_Path => Create (+Project_File),
               Env               => Env,
               Errors            => Print_Error'Access);
         exception
            when Invalid_Project =>
               Trace.Trace ("Loading failed");
               Cleanup;
               Abort_App;
         end;
         Trace.Trace ("Loading succeeded");
      end Load_Project;

      -------------------------
      -- Project_To_Provider --
      -------------------------

      function Project_To_Provider
        (Project : Project_Tree_Access) return Unit_Provider_Reference
      is
         Partition : Provider_And_Projects_Array_Access :=
            Create_Project_Unit_Providers (Project);
      begin
         --  Reject partitions with multiple parts: we cannot analyze it with
         --  only one provider.

         if Partition.all'Length /= 1 then
            Free (Partition);
            Abort_App ("This aggregate project contains conflicting sources");
         end if;

         return Result : constant Unit_Provider_Reference :=
            Partition.all (Partition'First).Provider
         do
            Free (Partition);
         end return;
      end Project_To_Provider;

      -------------------------------
      -- List_Sources_From_Project --
      -------------------------------

      procedure List_Sources_From_Project
        (Project : Project_Tree'Class; Files : out String_Vectors.Vector)
      is
         --  Get a sorted list of source files in Project's root project.
         --  Sorting gets the output deterministic and thus helps
         --  reproducibility.

         List : File_Array_Access := Project.Root_Project.Source_Files;
      begin
         Sort (List.all);

         for F of List.all loop
            declare
               FI        : constant File_Info := Project.Info (F);
               Full_Name : Filesystem_String renames F.Full_Name.all;
               Name      : constant String := +Full_Name;
            begin
               if FI.Language = "ada" then
                  Files.Append (+Name);
               end if;
            end;
         end loop;
         Unchecked_Free (List);
      end List_Sources_From_Project;

      ---------------------
      -- Files_From_Args --
      ---------------------

      function Files_From_Args
        (Files : out String_Vectors.Vector) return Boolean
      is
         Arg_Files : constant Args.Files.Result_Array := Args.Files.Get;
      begin
         if Arg_Files'Length = 0 then
            return False;
         else
            for F of Arg_Files loop
               Files.Append (F);
            end loop;
            return True;
         end if;
      end Files_From_Args;

      ---------
      -- Run --
      ---------

      procedure Run is

         procedure Finalize;
         --  Clean up local resources. This must be called both on normal
         --  termination and during abortion.

         Project : Project_Tree_Access;
         Env     : Project_Environment_Access;
         --  Reference to the loaded project tree, if any. Null otherwise.

         UFP : Unit_Provider_Reference;
         --  When project file handling is enabled, corresponding unit provider

         type App_Job_Context_Array_Access is access App_Job_Context_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (App_Job_Context_Array, App_Job_Context_Array_Access);

         App_Ctx      : aliased App_Context;
         Job_Contexts : App_Job_Context_Array_Access;

         Files : String_Vectors.Vector;
         Queue : String_Queues.Queue;

         task type Main_Task_Type is
            entry Start (ID : Job_ID);
            entry Stop;
         end Main_Task_Type;

         --------------
         -- Finalize --
         --------------

         procedure Finalize is
         begin
            UFP := No_Unit_Provider_Reference;
            if Project /= null then
               Project.Unload;
               Free (Project);
               Free (Env);
            end if;

            Free (Job_Contexts);
         end Finalize;

         --------------------
         -- Main_Task_Type --
         --------------------

         task body Main_Task_Type is
            F   : Unbounded_String;
            JID : Job_ID;
         begin
            --  Wait for the signal to start jobs

            accept Start (ID : Job_ID) do
               JID := ID;
            end Start;

            --  We can now do our processings and invoke user callbacks when
            --  appropriate.

            declare
               Job_Name : constant String := "Job" & JID'Image;
               Job_Ctx  : App_Job_Context renames Job_Contexts (JID);

               type Any_Step is (Setup, In_Unit, Post_Process);
               Step : Any_Step := Setup;
            begin
               Trace.Increase_Indent ("Setting up " & Job_Name);
               Job_Setup (Job_Ctx);
               Trace.Decrease_Indent;

               Step := In_Unit;
               loop
                  --  Stop as soon as we noticed that another job requested
                  --  abortion.

                  if Abortion.Abort_Signaled then
                     Trace.Trace
                       (Job_Name & " leaving after another job aborted");
                     Job_Ctx.Aborted := True;
                     exit;
                  end if;

                  --  Pick the next file and process it

                  select
                     Queue.Dequeue (F);
                  or
                     delay 0.1;
                     exit;
                  end select;

                  Trace.Increase_Indent (Job_Name & ": Processing " & (+F));
                  declare
                     Unit : constant Analysis_Unit :=
                        Job_Ctx.Analysis_Ctx.Get_From_File (+F);
                  begin
                     Process_Unit (Job_Ctx, Unit);
                     Job_Ctx.Units_Processed.Append (Unit);
                  end;
                  Trace.Decrease_Indent;

               end loop;

               Trace.Increase_Indent (Job_Name & ": Post-processing");
               Step := Post_Process;
               Job_Post_Process (Job_Ctx);
               Trace.Decrease_Indent;

            --  Make sure to handle properly uncaught errors (they have nowhere
            --  to propagate once here) and abortion requests.

            exception
               when Abort_App_Exception =>
                  Trace.Trace (Job_Name & " aborted the app");
                  Job_Ctx.Aborted := True;
                  Abortion.Signal_Abortion;

               when E : others =>
                  Job_Ctx.Aborted := True;
                  Abortion.Signal_Abortion;
                  declare
                     Context : constant String :=
                       (case Step is
                        when Setup        => "in Job_Setup",
                        when In_Unit      => "in Process_Unit for " & (+F),
                        when Post_Process => "in Job_Post_Process");
                  begin
                     Put_Line
                       (Standard_Error,
                        "Unhandled error " & Context
                        & " (job" & JID'Image & ")");
                     Dump_Exception (E);
                  end;
            end;

            accept Stop do
               null;
            end Stop;
         end Main_Task_Type;

      begin
         --  Setup traces from config file
         GNATCOLL.Traces.Parse_Config_File;

         if not Args.Parser.Parse then
            return;
         end if;

         Trace.Increase_Indent ("Setting up the unit provider");
         if Length (Args.Project_File.Get) > 0 then
            if Args.Auto_Dirs.Get'Length /= 0 then
               Abort_App ("--auto-dir conflicts with -P");
            end if;

            --  Handle project file and build the list of source files to
            --  process.
            Load_Project
              (Project_File  => +Args.Project_File.Get,
               Scenario_Vars => Args.Scenario_Vars.Get,
               Target        => +Args.Target.Get,
               RTS           => +Args.RTS.Get,
               Config_File   => +Args.Config_File.Get,
               Project       => Project,
               Env           => Env);
            UFP := Project_To_Provider (Project);
            if not Files_From_Args (Files) then
               List_Sources_From_Project (Project.all, Files);
            end if;

            --  Fill in the provider
            App_Ctx.Provider := (Kind => Project_File, Project => Project);

         elsif Args.Auto_Dirs.Get'Length > 0 then
            --  The auto provider is requested: initialize it with the given
            --  directories. Also build the list of source files to process.
            declare
               Auto_Dirs   : Args.Auto_Dirs.Result_Array renames
                  Args.Auto_Dirs.Get;
               Dirs        : GNATCOLL.VFS.File_Array (Auto_Dirs'Range);
               Found_Files : GNATCOLL.VFS.File_Array_Access;
            begin
               for I in Dirs'Range loop
                  Dirs (I) := Create (+To_String (Auto_Dirs (I)));
               end loop;
               Found_Files := Find_Files (Directories => Dirs);
               UFP := Create_Auto_Provider_Reference
                 (Found_Files.all, +Args.Charset.Get);

               if not Files_From_Args (Files) then
                  Sort (Found_Files.all);
                  for F of Found_Files.all loop
                     Files.Append (To_Unbounded_String (+F.Full_Name));
                  end loop;
               end if;

               --  Fill in the provider
               App_Ctx.Provider := (Kind => Auto_Dir, others => <>);
               for D of Dirs loop
                  App_Ctx.Provider.Dirs.Append
                    (To_Unbounded_String (+D.Full_Name));
               end loop;
               for F of Found_Files.all loop
                  App_Ctx.Provider.Dirs.Append
                    (To_Unbounded_String (+F.Full_Name));
               end loop;

               Unchecked_Free (Found_Files);
            end;

         else
            declare
               Dummy : Boolean := Files_From_Args (Files);
            begin
               --  Fill in the provider
               App_Ctx.Provider := (Kind => Default);
            end;
         end if;

         --  Make sure project-specific options are used only with -P
         if App_Ctx.Provider.Kind /= Project_File then
            if Args.Target.Get /= Null_Unbounded_String then
               Abort_App ("--target requires -P");
            elsif Args.RTS.Get /= Null_Unbounded_String then
               Abort_App ("--RTS requires -P");
            elsif Args.Config_File.Get /= Null_Unbounded_String then
               Abort_App ("--config requires -P");
            end if;
         end if;

         if Files.Is_Empty then
            Put_Line (Standard_Error, "No source file to process");
         end if;
         Trace.Decrease_Indent;

         --  Initialize contexts

         Job_Contexts := new App_Job_Context_Array'
           (1 .. Job_ID (Args.Jobs.Get) =>
            (App_Ctx => App_Ctx'Unchecked_Access, others => <>));
         for JID in Job_Contexts.all'Range loop
            Job_Contexts (JID) :=
              (ID              => JID,
               App_Ctx         => App_Ctx'Unchecked_Access,
               Analysis_Ctx    => Create_Context
                                    (Charset       => +Args.Charset.Get,
                                     Unit_Provider => UFP),
               Units_Processed => <>,
               Aborted         => False);
         end loop;

         --  Finally, create all jobs, and one context per job to process unit
         --  files.

         Trace.Trace ("Setting up the app");
         App_Setup (App_Ctx, Job_Contexts.all);

         Trace.Trace ("Running jobs");
         declare
            Task_Pool : array (Job_Contexts.all'Range) of Main_Task_Type;
         begin
            for JID in Task_Pool'Range loop
               Task_Pool (JID).Start (JID);
            end loop;

            for F of Files loop
               Queue.Enqueue (F);
            end loop;

            for T of Task_Pool loop
               T.Stop;
            end loop;
         end;

         --  If there is at least one job that triggered abortion, make sure
         --  the program stops with an error exit status. We still want to run
         --  post-processing in this case.

         if Abortion.Abort_Signaled then
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         end if;

         --  Run post-process routines and finalize the app

         Trace.Trace ("Running app post-processing");
         App_Post_Process (App_Ctx, Job_Contexts.all);
         Finalize;

         Trace.Trace ("Done");

      exception
         when Abort_App_Exception =>
            Trace.Trace ("App aborted");
            Finalize;
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end Run;
   end App;

end Libadalang.Helpers;
