with Ada.Calendar;          use Ada.Calendar;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Traceback.Symbolic;

with GNATCOLL.JSON;
with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Adalog.Debug;   use Langkit_Support.Adalog.Debug;
with Langkit_Support.Slocs;          use Langkit_Support.Slocs;
with Langkit_Support.Text;           use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Helpers;   use Libadalang.Helpers;
with Libadalang.Iterators; use Libadalang.Iterators;

with Put_Title;

procedure Nameres is

   package String_Vectors is new Ada.Containers.Vectors
     (Positive, Unbounded_String);

   package J renames GNATCOLL.JSON;

   package Stats_Data is
      Nb_Files_Analyzed    : Natural := 0;
      Nb_Successes         : Natural := 0;
      Nb_Fails             : Natural := 0;
      Nb_Xfails            : Natural := 0;
      Nb_Exception_Fails   : Natural := 0;
      Max_Nb_Fails         : Natural := 0;
      File_With_Most_Fails : Unbounded_String;
   end Stats_Data;

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array);
   procedure Job_Setup (Context : App_Job_Context);
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);
   procedure App_Tear_Down
     (Context : App_Context; Jobs : App_Job_Context_Array);

   package App is new Libadalang.Helpers.App
     (Description   =>
         "Run Libadalang's name resolution on a file, set of files or project",
      App_Setup     => App_Setup,
      Job_Setup     => Job_Setup,
      Process_Unit  => Process_Unit,
      App_Tear_Down => App_Tear_Down);

   package Args is
      use GNATCOLL.Opt_Parse;

      package File_Limit is new Parse_Option
        (App.Args.Parser, "-l", "--file-limit", "Stop program after N files",
         Integer, Default_Val => -1);

      package Discard_Errors_In_PLE is new Parse_Flag
        (App.Args.Parser, "-D", "--discard-PLE-errors",
         "Discard errors while constructing lexical envs");

      package Quiet is new Parse_Flag
        (App.Args.Parser, "-q", "--quiet", "Quiet mode (no output on stdout)");

      package JSON is new Parse_Flag
        (App.Args.Parser, "-J", "--json",
         "JSON mode (Structured output on stdout)");

      package Stats is new Parse_Flag
        (App.Args.Parser, "-S", "--stats",
         "Output stats at the end of analysis");

      package Resolve_All is new Parse_Flag
        (App.Args.Parser, "-A", "--all", "Resolve every cross reference");

      package Solve_Line is new Parse_Option
        (App.Args.Parser, "-L", "--solve-line", "Only analyze line N",
         Natural, Default_Val => 0);

      package Only_Show_Failures is new Parse_Flag
        (App.Args.Parser, Long => "--only-show-failures",
         Help => "Only output failures on stdout");

      package Imprecise_Fallback is new Parse_Flag
        (App.Args.Parser, Long => "--imprecise-fallback",
         Help => "Activate fallback mechanism for name resolution");

      package Disable_Operator_Resolution is new Parse_Flag
        (App.Args.Parser, Long => "--disable-operator-resolution",
         Help => "Do not resolve unary/binary operations");

      package Dump_Envs is new Parse_Flag
        (App.Args.Parser, "-E", "--dump-envs",
         Help => "Dump lexical envs after populating them");

      package Do_Reparse is new Parse_Flag
        (App.Args.Parser,
         Long => "--reparse",
         Help => "Reparse units 10 times (hardening)");

      package Time is new Parse_Flag
        (App.Args.Parser,
         Long => "--time",
         Short => "-T",
         Help => "Show the time it took to nameres each file. "
                 & "Implies --quiet so that you only have timing info");

      package Timeout is new Parse_Option
        (App.Args.Parser, "-t", "--timeout",
         "Timeout equation solving after N steps",
         Natural, Default_Val => 100_000);

      package No_Lookup_Cache is new Parse_Flag
        (App.Args.Parser,
         Long => "--no-lookup-cache", Help => "Deactivate lookup cache");

      package Trace is new Parse_Flag
        (App.Args.Parser, "-T", "--trace",
         Help => "Trace logic equation solving");

      package Debug is new Parse_Flag
        (App.Args.Parser, "-D", "--debug",
         Help => "Debug logic equation solving");
   end Args;

   function Quiet return Boolean is
      (Args.Quiet.Get or else Args.JSON.Get or else Args.Time.Get);

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
      Obj : in out J.JSON_Value);
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
      Obj : in out J.JSON_Value) is
   begin
      App.Dump_Exception (E);

      if Args.JSON.Get then
         Obj.Set_Field ("success", False);
         Obj.Set_Field
           ("exception_message", Ada.Exceptions.Exception_Message (E));

         if App.Args.Sym_Traceback.Get then
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

   ---------------
   -- App_Setup --
   ---------------

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array) is
      pragma Unreferenced (Context, Jobs);
   begin
      if Args.No_Lookup_Cache.Get then
         Disable_Lookup_Cache (True);
      end if;

      if Args.Trace.Get then
         Set_Debug_State (Trace);
      elsif Args.Debug.Get then
         Set_Debug_State (Step);
      end if;
   end App_Setup;

   ---------------
   -- Job_Setup --
   ---------------

   procedure Job_Setup (Context : App_Job_Context) is
      Ctx : Analysis_Context renames Context.Analysis_Ctx;
   begin
      Ctx.Discard_Errors_In_Populate_Lexical_Env
        (Args.Discard_Errors_In_PLE.Get);
      Ctx.Set_Logic_Resolution_Timeout (Args.Timeout.Get);
   end Job_Setup;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);

      Basename : constant String :=
         +Create (+Unit.Get_Filename).Base_Name;

      Before, After : Time;
      Time_Elapsed  : Duration;
   begin
      if not Quiet then
         Put_Title ('#', "Analyzing " & Basename);
      end if;
      Before := Clock;

      begin
         Process_File (Unit, Unit.Get_Filename);
      exception
         when E : others =>
            Put_Line ("PLE failed with exception for file " & Basename);
            App.Dump_Exception (E);
            return;
      end;
      After := Clock;

      Time_Elapsed := After - Before;

      if Args.Time.Get then
         Ada.Text_IO.Put_Line
           ("Time elapsed in process file for "
            & Basename & ": " & Time_Elapsed'Image);
      end if;

      Stats_Data.Nb_Files_Analyzed := Stats_Data.Nb_Files_Analyzed + 1;
      if Args.File_Limit.Get /= -1
         and then Stats_Data.Nb_Files_Analyzed
                  >= Args.File_Limit.Get
      then
         Abort_App ("Requested file limit reached: aborting");
      end if;
   end Process_Unit;

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

         procedure Resolve_Entry_Point (Node : Ada_Node);

         -------------------------
         -- Resolve_Entry_Point --
         -------------------------

         procedure Resolve_Entry_Point (Node : Ada_Node) is
         begin
            if Node /= Block then
               Resolve_Node (Node);
            end if;
         end Resolve_Entry_Point;

         It : Ada_Node_Iterators.Iterator'Class :=
            Find (Block, Is_Xref_Entry_Point'Access);
      begin
         It.Iterate (Resolve_Entry_Point'Access);
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
            Dump_Exception (E, Obj);

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

   -------------------
   -- App_Tear_Down --
   -------------------

   procedure App_Tear_Down
     (Context : App_Context; Jobs : App_Job_Context_Array)
   is
      pragma Unreferenced (Context, Jobs);
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

      Put_Line ("Done.");
   end App_Tear_Down;

begin
   App.Run;
end Nameres;
