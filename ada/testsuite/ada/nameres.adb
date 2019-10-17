with Ada.Calendar;          use Ada.Calendar;
with Ada.Command_Line;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Traceback.Symbolic;

with GNATCOLL.JSON;
with GNATCOLL.Opt_Parse;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.Traces;
with GNATCOLL.VFS;       use GNATCOLL.VFS;

with Langkit_Support.Adalog.Debug;   use Langkit_Support.Adalog.Debug;
with Langkit_Support.Slocs;          use Langkit_Support.Slocs;
with Langkit_Support.Text;           use Langkit_Support.Text;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Auto_Provider;    use Libadalang.Auto_Provider;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Iterators;        use Libadalang.Iterators;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

with Put_Title;

procedure Nameres is

   package String_Vectors is new Ada.Containers.Vectors
     (Positive, Unbounded_String);

   package String_QI
   is new Ada.Containers.Synchronized_Queue_Interfaces (Unbounded_String);

   package String_Queues
   is new Ada.Containers.Unbounded_Synchronized_Queues (String_QI);

   package J renames GNATCOLL.JSON;

   Queue : String_Queues.Queue;

   pragma Warnings (Off, "ref");

   package Stats_Data is
      Nb_Files_Analyzed    : Natural := 0;
      Nb_Successes         : Natural := 0;
      Nb_Fails             : Natural := 0;
      Nb_Xfails            : Natural := 0;
      Nb_Exception_Fails   : Natural := 0;
      Max_Nb_Fails         : Natural := 0;
      File_With_Most_Fails : Unbounded_String;
   end Stats_Data;

   package Args is
      use GNATCOLL.Opt_Parse;
      Parser : Argument_Parser := Create_Argument_Parser
        (Help =>
           "Run Libadalang's name resolution on a file, set of file, "
           & "or project");

      package File_Limit is new Parse_Option
        (Parser, "-l", "--file-limit", "Stop program after N files", Integer,
         Default_Val => -1);

      package Charset is new Parse_Option
        (Parser, "-C", "--charset", "Charset to use for source decoding",
         Unbounded_String, Default_Val => To_Unbounded_String ("iso-8859-1"));

      package Discard_Errors_In_PLE is new Parse_Flag
        (Parser, "-D", "--discard-PLE-errors",
         "Discard errors while constructing lexical envs");

      package Quiet is new Parse_Flag
        (Parser, "-q", "--quiet", "Quiet mode (no output on stdout)");

      package JSON is new Parse_Flag
        (Parser, "-J", "--json", "JSON mode (Structured output on stdout)");

      package Stats is new Parse_Flag
        (Parser, "-S", "--stats", "Output stats at the end of analysis");

      package Resolve_All is new Parse_Flag
        (Parser, "-A", "--all", "Resolve every cross reference");

      package Solve_Line is new Parse_Option
        (Parser, "-L", "--solve-line", "Only analyze line N",
         Natural, Default_Val => 0);

      package Only_Show_Failures is new Parse_Flag
        (Parser, Long => "--only-show-failures",
         Help => "Only output failures on stdout");

      package Imprecise_Fallback is new Parse_Flag
        (Parser, Long => "--imprecise-fallback",
         Help => "Activate fallback mechanism for name resolution");

      package Disable_Operator_Resolution is new Parse_Flag
        (Parser, Long => "--disable-operator-resolution",
         Help => "Do not resolve unary/binary operations");

      package Dump_Envs is new Parse_Flag
        (Parser, "-E", "--dump-envs",
         Help => "Dump lexical envs after populating them");

      package Do_Reparse is new Parse_Flag
        (Parser,
         Long => "--reparse",
         Help => "Reparse units 10 times (hardening)");

      package Time is new Parse_Flag
        (Parser,
         Long => "--time",
         Short => "-T",
         Help => "Show the time it took to nameres each file. "
                 & "Implies --quiet so that you only have timing info");

      package Timeout is new Parse_Option
        (Parser, "-t", "--timeout", "Timeout equation solving after N steps",
         Natural, Default_Val => 100_000);

      package Jobs is new Parse_Option
        (Parser, "-j", "--jobs", "Number of parallel jobs to use",
         Natural, Default_Val => 1);

      package No_Lookup_Cache is new Parse_Flag
        (Parser,
         Long => "--no-lookup-cache", Help => "Deactivate lookup cache");

      package Trace is new Parse_Flag
        (Parser, "-T", "--trace", Help => "Trace logic equation solving");

      package Debug is new Parse_Flag
        (Parser, "-D", "--debug", Help => "Debug logic equation solving");

      package No_Traceback is new Parse_Flag
        (Parser, Long => "--no-traceback",
         Help         => "Do not display traceback for exceptions");

      package Sym_Traceback is new Parse_Flag
        (Parser, Long => "--symbolic-traceback",
         Help         => "Show symbolic tracebacks for exceptions");

      package Files_From_Project is new Parse_Flag
        (Parser,
         Long => "--files-from-project",
         Help => "Take files from specified project file");

      package Auto_Dirs is new Parse_Option_List
        (Parser, "-A", "--auto-dir",
         Arg_Type   => Unbounded_String,
         Accumulate => True,
         Help       =>
            "Directories to use for the auto provider. If one is passed, "
            & "auto provider will be used, and project options ignored");

      package Scenario_Vars is new Parse_Option_List
        (Parser, Short => "-X", Long => "--scenario-variables",
         Arg_Type   => Unbounded_String,
         Accumulate => True,
         Help       => "Scenario variables to pass to the project file");

      package Project_File is new Parse_Option
        (Parser, "-P", "--project",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "Project file to use");

      package Files is new Parse_Positional_Arg_List
        (Parser,
         Name        => "files",
         Arg_Type    => Unbounded_String,
         Help        => "Files to analyze",
         Allow_Empty => True);
   end Args;

   function Quiet return Boolean is
      (Args.Quiet.Get or else Args.JSON.Get or else Args.Time.Get);

   UFP : Unit_Provider_Reference;
   --  When project file handling is enabled, corresponding unit provider

   function Text (N : Ada_Node'Class) return String is (Image (Text (N)));

   function "+" (S : String) return Unbounded_String
      renames To_Unbounded_String;
   function "+" (S : Unbounded_String) return String renames To_String;

   function "<" (Left, Right : Ada_Node) return Boolean is
     (Sloc_Range (Left).Start_Line < Sloc_Range (Right).Start_Line);

   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Ada_Node,
      Array_Type   => Ada_Node_Array,
      "<"          => "<");

   function Decode_Boolean_Literal (T : Text_Type) return Boolean is
     (Boolean'Wide_Wide_Value (T));
   procedure Process_File (Unit : Analysis_Unit; Filename : String);

   procedure Add_Files_From_Project
     (PT    : Project_Tree_Access;
      P     : Project_Type;
      Files : in out String_Vectors.Vector);

   procedure Show_Stats;

   function Do_Pragma_Test (Arg : Expr) return Ada_Node_Array is
     (P_Matching_Nodes (Arg));
   --  Do the resolution associated to a Test pragma.
   --
   --  This function is here to provide a simple breakpoint location for
   --  debugging sessions.

   procedure New_Line;
   procedure Put_Line (S : String);
   procedure Put (S : String);

   procedure Dump_Exception
     (E   : Ada.Exceptions.Exception_Occurrence;
      Obj : access J.JSON_Value := null);
   --  Dump the exception ``E``, honoring the ``Args.No_Traceback`` flag (eg.
   --  don't show tracebacks when asked not to). If ``Obj`` is passed and
   --  ``Args.JSON`` is set, also set fields in ``Obj``.

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      if not Quiet then
         Ada.Text_IO.New_Line;
      end if;
   end New_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is begin
      if not Quiet then
         Ada.Text_IO.Put_Line (S);
      end if;
   end Put_Line;

   ---------
   -- Put --
   ---------

   procedure Put (S : String) is begin
      if not Quiet then
         Ada.Text_IO.Put (S);
      end if;
   end Put;

   --------------------
   -- Dump_Exception --
   --------------------

   procedure Dump_Exception
     (E   : Ada.Exceptions.Exception_Occurrence;
      Obj : access J.JSON_Value := null)
   is
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

      if Args.JSON.Get then
         Obj.Set_Field ("success", False);
         Obj.Set_Field
           ("exception_message", Ada.Exceptions.Exception_Message (E));

         if Args.Sym_Traceback.Get then
            Obj.Set_Field
              ("exception_traceback",
               GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         else
            Obj.Set_Field
              ("exception_traceback",
               Ada.Exceptions.Exception_Information (E));
         end if;

         Ada.Text_IO.Put_Line (Obj.Write);
      end if;
   end Dump_Exception;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (Unit : Analysis_Unit; Filename : String) is

      Nb_File_Fails : Natural := 0;

      procedure Resolve_Node (Node : Ada_Node; Show_Slocs : Boolean := True);
      procedure Resolve_Block (Block : Ada_Node);

      -------------------
      -- Resolve_Block --
      -------------------

      procedure Resolve_Block (Block : Ada_Node) is
         function Is_Xref_Entry_Point (N : Ada_Node) return Boolean
         is (P_Xref_Entry_Point (N)
             and then
               (Args.Solve_Line.Get = 0
                or else
                Natural (Sloc_Range (N).Start_Line) = Args.Solve_Line.Get));
      begin
         for Node of Find (Block, Is_Xref_Entry_Point'Access).Consume loop
            if Node /= Block then
               Resolve_Node (Node);
            end if;
         end loop;
      end Resolve_Block;

      ------------------
      -- Resolve_Node --
      ------------------

      procedure Resolve_Node (Node : Ada_Node; Show_Slocs : Boolean := True) is

         function XFAIL return Boolean;
         --  If there is an XFAIL pragma for the node being resolved, show the
         --  message, and return True.

         function Print_Node (N : Ada_Node'Class) return Visit_Status;

         ----------------
         -- Print_Node --
         ----------------

         function Print_Node (N : Ada_Node'Class) return Visit_Status is
         begin
            if not Quiet
               and then Kind (N) in Ada_Expr
               and then (Kind (N) not in Ada_Name
                         or else not N.As_Name.P_Is_Defining)
            then
               Put_Line ("Expr: " & N.Short_Image);
               if Kind (N) in Ada_Name
                  and then
                    (not Args.Disable_Operator_Resolution.Get
                     or else not (Kind (N) in Ada_Op))
               then
                  declare
                     Decl_Name : constant Defining_Name :=
                        N.As_Name.P_Referenced_Defining_Name
                          (Args.Imprecise_Fallback.Get);

                     Referenced_Decl_Image : constant String :=
                        (if Show_Slocs
                         then Image (Decl_Name)
                         else Image
                           (Decl_Name.P_Basic_Decl
                            .P_Unique_Identifying_Name));
                  begin
                     Put_Line ("  references: " & Referenced_Decl_Image);
                  end;
               end if;

               declare
                  Decl : constant Base_Type_Decl :=
                     P_Expression_Type (As_Expr (N));

                  Decl_Image : constant String :=
                    (if Show_Slocs or else Decl.Is_Null
                     then Image (Decl)
                     else Image (Decl.P_Unique_Identifying_Name));
               begin
                  Put_Line ("  type:       " & Decl_Image);
               end;
            end if;
            return
              (if (P_Xref_Entry_Point (N) and then As_Ada_Node (N) /= Node)
               or else (Kind (N) in Ada_Defining_Name
                        and then not P_Xref_Entry_Point (N))
               then Over
               else Into);
         end Print_Node;

         Dummy : Visit_Status;

         Obj : aliased J.JSON_Value;

         -----------
         -- XFAIL --
         -----------

         function XFAIL return Boolean is
            N : constant Ada_Node := Next_Sibling (Node);
         begin
            if not Is_Null (N) and then Kind (N) = Ada_Pragma_Node then
               if Child (N, 1).Text = "XFAIL_Nameres" then
                  declare
                     Arg : constant String_Literal :=
                        N.As_Pragma_Node.F_Args.Child (1)
                        .As_Base_Assoc.P_Assoc_Expr.As_String_Literal;
                  begin
                     if Arg.Is_Null then
                        raise Program_Error
                          with "Invalid arg for " & N.Short_Image;
                     end if;
                     Put_Line ("XFAIL: " & Image (Arg.P_Denoted_Value, False));
                     Put_Line ("");
                  end;
               end if;
               return True;
            end if;
            return False;
         end XFAIL;
      begin
         if Args.JSON.Get then
            Obj := J.Create_Object;
            Obj.Set_Field ("kind", "node_resolution");
         end if;

         if not (Quiet or else Args.Only_Show_Failures.Get) then
            Put_Title ('*', "Resolving xrefs for node " & Node.Short_Image);
         end if;
         if Langkit_Support.Adalog.Debug.Debug then
            Assign_Names_To_Logic_Vars (Node);
         end if;

         if Args.JSON.Get then
            Obj.Set_Field ("file", Filename);
            Obj.Set_Field ("sloc", Image (Node.Sloc_Range));
         end if;

         if P_Resolve_Names (Node) or else Args.Imprecise_Fallback.Get then
            if not Args.Only_Show_Failures.Get then
               Dummy := Traverse (Node, Print_Node'Access);
            end if;

            Stats_Data.Nb_Successes := Stats_Data.Nb_Successes + 1;

            if Args.JSON.Get then
               Obj.Set_Field ("success", True);
            end if;
         else
            Put_Line ("Resolution failed for node " & Node.Short_Image);
            if XFAIL then
               Stats_Data.Nb_Xfails := Stats_Data.Nb_Xfails + 1;
            else
               Nb_File_Fails := Nb_File_Fails + 1;
            end if;

            if Args.JSON.Get then
               Obj.Set_Field ("success", False);
            end if;
         end if;

         if not (Quiet or else Args.Only_Show_Failures.Get) then
            Put_Line ("");
         end if;

         if Args.JSON.Get then
            Ada.Text_IO.Put_Line (Obj.Write);
         end if;
      exception
         when E : others =>
            Put_Line
              ("Resolution failed with exception for node "
               & Node.Short_Image);
            Dump_Exception (E, Obj'Access);

            if XFAIL then
               Stats_Data.Nb_Xfails := Stats_Data.Nb_Xfails + 1;
            else
               Stats_Data.Nb_Exception_Fails :=
                 Stats_Data.Nb_Exception_Fails + 1;
               Nb_File_Fails := Nb_File_Fails + 1;
            end if;
      end Resolve_Node;

   begin
      if Has_Diagnostics (Unit) then
         for D of Diagnostics (Unit) loop
            Put_Line (Format_GNU_Diagnostic (Unit, D));
         end loop;
         return;
      end if;
      Populate_Lexical_Env (Unit);

      if Args.Do_Reparse.Get then
         for I in 1 .. 10 loop
            Reparse (Unit);
            Populate_Lexical_Env (Unit);
         end loop;
      end if;

      if Args.Dump_Envs.Get then
         Put_Title ('-', "Dumping envs for " & Filename);
         Dump_Lexical_Env (Unit);
      end if;

      if Args.Resolve_All.Get or Args.Solve_Line.Get /= 0 then
         Resolve_Block (Root (Unit));
      end if;

      declare
         --  Configuration for this unit
         Display_Slocs        : Boolean := False;
         Display_Short_Images : Boolean := False;

         Empty     : Boolean := True;
         P_Node    : Pragma_Node;

         function Is_Pragma_Node (N : Ada_Node) return Boolean is
           (Kind (N) = Ada_Pragma_Node);

         function Pragma_Name return String is (Text (F_Id (P_Node)));

         procedure Handle_Pragma_Config (Id : Identifier; E : Expr);

         --------------------------
         -- Handle_Pragma_Config --
         --------------------------

         procedure Handle_Pragma_Config (Id : Identifier; E : Expr) is
            Name : constant Text_Type := Text (Id);
         begin
            if Kind (E) /= Ada_Identifier then
               raise Program_Error with
                 ("Invalid config value for " & Image (Name, True) & ": "
                  & Text (E));
            end if;

            declare
               Value : constant Text_Type := Text (E);
            begin
               if Name = "Display_Slocs" then
                  Display_Slocs := Decode_Boolean_Literal (Value);
               elsif Name = "Display_Short_Images" then
                  Display_Short_Images := Decode_Boolean_Literal (Value);
               else
                  raise Program_Error with
                    ("Invalid configuration: " & Image (Name, True));
               end if;
            end;
         end Handle_Pragma_Config;

      begin
         --  Print what entities are found for expressions X in all the "pragma
         --  Test (X)" we can find in this unit.
         for Node of Find (Root (Unit), Is_Pragma_Node'Access).Consume loop

            P_Node := As_Pragma_Node (Node);

            if Pragma_Name = "Config" then
               --  Handle testcase configuration pragmas for this file
               for Arg of Ada_Node_Array'(Children (F_Args (P_Node))) loop
                  declare
                     A : constant Pragma_Argument_Assoc :=
                        As_Pragma_Argument_Assoc (Arg);
                  begin
                     if Is_Null (F_Id (A)) then
                        raise Program_Error with
                           "Name missing in pragma Config";
                     elsif Kind (F_Id (A)) /= Ada_Identifier then
                        raise Program_Error with
                           "Name in pragma Config must be an identifier";
                     end if;

                     Handle_Pragma_Config (F_Id (A), F_Expr (A));
                  end;
               end loop;

            elsif Pragma_Name = "Section" then
               --  Print headlines
               declare
                  pragma Assert (Children_Count (F_Args (P_Node)) = 1);
                  Arg : constant Expr :=
                     P_Assoc_Expr (As_Base_Assoc (Child (F_Args (P_Node), 1)));
                  pragma Assert (Kind (Arg) = Ada_String_Literal);

                  T : constant Text_Type := Text (Arg);
               begin
                  if not Quiet then
                     Put_Title ('-', Image (T (T'First + 1 .. T'Last - 1)));
                  end if;
               end;
               Empty := True;

            elsif Pragma_Name = "Test" then
               --  Perform name resolution
               declare
                  pragma Assert (Children_Count (F_Args (P_Node)) in 1 | 2);

                  Arg      : constant Expr :=
                     P_Assoc_Expr (As_Base_Assoc (Child (F_Args (P_Node), 1)));

                  Debug_Lookup : Boolean := False;

               begin
                  if Children_Count (F_Args (P_Node)) = 2 then
                     Debug_Lookup := Text
                       (P_Assoc_Expr (As_Base_Assoc
                         (Child (F_Args (P_Node), 2))))
                          = String'("Debug");
                  end if;

                  Trigger_Envs_Debug (Debug_Lookup);

                  declare
                     Entities : Ada_Node_Array := Do_Pragma_Test (Arg);
                  begin

                     Put_Line (Text (Arg) & " resolves to:");
                     Sort (Entities);
                     for E of Entities loop
                        Put ("    " & (if Display_Short_Images
                                       then E.Short_Image
                                       else Text (E)));
                        if Display_Slocs then
                           Put_Line (" at "
                                     & Image (Start_Sloc (Sloc_Range (E))));
                        else
                           New_Line;
                        end if;
                     end loop;
                     if Entities'Length = 0 then
                        Put_Line ("    <none>");
                     end if;
                  end;
               end;

               Empty := False;
               Trigger_Envs_Debug (False);

            elsif Pragma_Name = "Test_Statement" then
               pragma Assert (Children_Count (F_Args (P_Node)) = 0);
               Resolve_Node (Previous_Sibling (P_Node));
               Empty := False;

            elsif Pragma_Name = "Test_Statement_UID" then

               --  Like Test_Statement, but will show the unique_identifying
               --  name of the declarations instead of the node image.
               --  This is used in case the node might change (for example in
               --  tests where we resolve runtime things).

               pragma Assert (Children_Count (F_Args (P_Node)) = 0);
               Resolve_Node (Previous_Sibling (P_Node), Show_Slocs => False);
               Empty := False;

            elsif Pragma_Name = "Test_Block" then
               pragma Assert (Children_Count (F_Args (P_Node)) = 0);
               declare
                  Parent_1 : constant Ada_Node := Parent (P_Node);
                  Parent_2 : constant Ada_Node := Parent (Parent_1);
               begin
                  Resolve_Block
                    (if Kind (Parent_2) = Ada_Compilation_Unit
                     then F_Body (As_Compilation_Unit (Parent_2))
                     else Previous_Sibling (P_Node));
               end;
               Empty := False;
            end if;
         end loop;
         if not Empty then
            New_Line;
         end if;
      end;

      Stats_Data.Nb_Fails := Stats_Data.Nb_Fails + Nb_File_Fails;
      if Stats_Data.Max_Nb_Fails < Nb_File_Fails then
         Stats_Data.File_With_Most_Fails := +Filename;
         Stats_Data.Max_Nb_Fails := Nb_File_Fails;
      end if;

   end Process_File;

   ----------------------------
   -- Add_Files_From_Project --
   ----------------------------

   procedure Add_Files_From_Project
     (PT    : Project_Tree_Access;
      P     : Project_Type;
      Files : in out String_Vectors.Vector)
   is
      List : File_Array_Access := P.Source_Files;
   begin
      Sort (List.all);

      for F of List.all loop
         declare
            FI        : constant File_Info := PT.Info (F);
            Full_Name : Filesystem_String renames F.Full_Name.all;
            Name      : constant String := +Full_Name;
         begin
            if FI.Language = "ada" then
               Files.Append (+Name);
            end if;
         end;
      end loop;
      Unchecked_Free (List);
   end Add_Files_From_Project;

   ----------------
   -- Show_Stats --
   ----------------

   procedure Show_Stats is
      Total : constant Natural :=
        Stats_Data.Nb_Successes + Stats_Data.Nb_Fails + Stats_Data.Nb_Xfails;
   begin
      if Args.Stats.Get and then Total > 0 then
         declare
            type Percentage is delta 0.01 range 0.0 .. 0.01 * 2.0**32;
            Percent_Successes : constant Percentage := Percentage
              (Float (Stats_Data.Nb_Successes) / Float (Total) * 100.0);
            Percent_Failures : constant Percentage :=
              Percentage (Float (Stats_Data.Nb_Fails) / Float (Total) * 100.0);

            Percent_XFAIL : constant Percentage :=
              Percentage
                (Float (Stats_Data.Nb_Xfails) / Float (Total) * 100.0);
         begin
            Put_Line ("Resolved " & Total'Image & " nodes");
            Put_Line ("Of which" & Stats_Data.Nb_Successes'Image
                      & " successes - " & Percent_Successes'Image & "%");
            Put_Line ("Of which" & Stats_Data.Nb_Fails'Image
                      & " failures - " & Percent_Failures'Image & "%");

            Put_Line ("Of which" & Stats_Data.Nb_Xfails'Image
                      & " XFAILS - " & Percent_XFAIL'Image & "%");
            Put_Line ("File with most failures ("
                      & Stats_Data.Max_Nb_Fails'Image
                      & "):" & (+Stats_Data.File_With_Most_Fails));
         end;
      end if;
   end Show_Stats;

   Files : String_Vectors.Vector;

   task type Main_Task_Type is
      entry Create_Context (UFP : Unit_Provider_Reference);
      entry Stop;
   end Main_Task_Type;

   --------------------
   -- Main_Task_Type --
   --------------------

   task body Main_Task_Type is
      Ctx : Analysis_Context;
      F   : Unbounded_String;
   begin
      select
         accept Create_Context (UFP : Unit_Provider_Reference) do
            Ctx := Create_Context
              (Charset       => +Args.Charset.Get,
               Unit_Provider => UFP);

            Discard_Errors_In_Populate_Lexical_Env
              (Ctx, Args.Discard_Errors_In_PLE.Get);

            Set_Logic_Resolution_Timeout (Ctx, Args.Timeout.Get);
         end Create_Context;
      or
         --  If the main task could not call the Create_Context entry (because
         --  it raised an exception, for instance), terminate gracefully.
         terminate;
      end select;

      loop
         select
            Queue.Dequeue (F);
         or
            delay 0.1;
            exit;
         end select;

         declare
            File     : constant String := +F;
            Basename : constant String :=
              +Create (+File).Base_Name;
            Unit     : Analysis_Unit;
            Before, After : Time;
            Time_Elapsed  : Duration;
         begin
            Unit := Get_From_File (Ctx, File);

            if not Quiet then
               Put_Title ('#', "Analyzing " & Basename);
            end if;
            Before := Clock;
            Process_File (Unit, File);
            After := Clock;

            Time_Elapsed := After - Before;

            if Args.Time.Get then
               Ada.Text_IO.Put_Line
                 ("Time elapsed in process file for "
                  & Basename & ": " & Time_Elapsed'Image);
            end if;

            Stats_Data.Nb_Files_Analyzed
              := Stats_Data.Nb_Files_Analyzed + 1;
            exit when Args.File_Limit.Get /= -1
              and then Stats_Data.Nb_Files_Analyzed
                >= Args.File_Limit.Get;
         exception
            when E : others =>
               Put_Line
                 ("PLE failed with exception for file " & Simple_Name (File));
               Dump_Exception (E, null);
         end;
      end loop;

      accept Stop do
         null;
      end Stop;
   end Main_Task_Type;

begin

   --  Setup traces from config file
   GNATCOLL.Traces.Parse_Config_File;

   --
   --  Set up of file & project data from command line arguments
   --

   if not Args.Parser.Parse then
      return;
   end if;

   declare
      Task_Pool : array (0 .. Args.Jobs.Get - 1) of Main_Task_Type;
   begin

      if Args.No_Lookup_Cache.Get then
         Disable_Lookup_Cache (True);
      end if;

      if Args.Trace.Get then
         Set_Debug_State (Trace);
      elsif Args.Debug.Get then
         Set_Debug_State (Step);
      end if;

      if not Args.Files_From_Project.Get then
         for F of Args.Files.Get loop
            Files.Append (F);
         end loop;
      end if;

      if Length (Args.Project_File.Get) > 0 then
         declare
            Filename : constant String := +Args.Project_File.Get;
            Env      : Project_Environment_Access;
            Project  : constant Project_Tree_Access := new Project_Tree;
         begin
            Initialize (Env);

            --  Set scenario variables
            for Assoc of Args.Scenario_Vars.Get loop
               declare
                  A        : constant String := +Assoc;
                  Eq_Index : Natural := A'First;
               begin
                  while Eq_Index <= A'Length and then A (Eq_Index) /= '=' loop
                     Eq_Index := Eq_Index + 1;
                  end loop;
                  if Eq_Index not in A'Range then
                     Put_Line ("Invalid scenario variable: -X" & A);
                     Ada.Command_Line.Set_Exit_Status
                       (Ada.Command_Line.Failure);
                     return;
                  end if;
                  Change_Environment
                    (Env.all,
                     A (A'First .. Eq_Index - 1),
                     A (Eq_Index + 1 .. A'Last));
               end;
            end loop;

            Load (Project.all, Create (+Filename), Env);
            UFP := Create_Project_Unit_Provider_Reference (Project, Env);

            if Args.Files_From_Project.Get then
               Add_Files_From_Project (Project, Project.Root_Project, Files);
            end if;
         end;

      elsif Args.Auto_Dirs.Get'Length > 0 then
         declare
            Auto_Dirs : Args.Auto_Dirs.Result_Array renames Args.Auto_Dirs.Get;
            Dirs      : GNATCOLL.VFS.File_Array (Auto_Dirs'Range);
            Files     : GNATCOLL.VFS.File_Array_Access;
         begin
            for I in Dirs'Range loop
               Dirs (I) := Create (+To_String (Auto_Dirs (I)));
            end loop;

            Files := Find_Files (Directories => Dirs);

            UFP := Create_Auto_Provider_Reference
              (Files.all, +Args.Charset.Get);
            GNATCOLL.VFS.Unchecked_Free (Files);
         end;
      end if;

      --
      --  Main logic
      --

      for T of Task_Pool loop
         T.Create_Context (UFP);
      end loop;

      for F of Files loop
         Queue.Enqueue (F);
      end loop;

      for T of Task_Pool loop
         T.Stop;
      end loop;
   end;

   Show_Stats;

   Put_Line ("Done.");
exception
   when others =>
      Show_Stats;
      raise;
end Nameres;
