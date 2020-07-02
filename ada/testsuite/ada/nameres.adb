with Ada.Calendar;                    use Ada.Calendar;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

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

   type Stats_Record is record
      Nb_Files_Analyzed    : Natural := 0;
      Nb_Successes         : Natural := 0;
      Nb_Fails             : Natural := 0;
      Nb_Xfails            : Natural := 0;
      Nb_Exception_Fails   : Natural := 0;
      Max_Nb_Fails         : Natural := 0;
      File_With_Most_Fails : Unbounded_String;
   end record;

   procedure Merge (Stats : in out Stats_Record; Other : Stats_Record);
   --  Merge data from Stats and Other into Stats

   type Config_Record is record
      Display_Slocs : Boolean := False;
      --  Whether to display slocs for resolved names

      Display_Images : Boolean := False;
      --  Whether to display images for resolved names
   end record;

   type Reference_Kind is (Any, Subp_Call, Subp_Overriding, Type_Derivation);
   --  Kind of reference for Find_All_References pragmas. Each kind leads to a
   --  specific find-all-references property: see Process_Refs below for
   --  associations.

   type Refs_Request is record
      Kind               : Reference_Kind;
      Target             : Basic_Decl;
      Imprecise_Fallback : Boolean;
      Show_Slocs         : Boolean;
      From_Pragma        : Pragma_Node;
   end record;

   package Refs_Request_Vectors is new Ada.Containers.Vectors
     (Positive, Refs_Request);

   type Job_Data_Record is record
      Stats         : Stats_Record;
      Config        : Config_Record;
      Refs_Requests : Refs_Request_Vectors.Vector;
   end record;

   type Job_Data_Array is array (Job_ID range <>) of Job_Data_Record;
   type Job_Data_Array_Access is access all Job_Data_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Job_Data_Array, Job_Data_Array_Access);

   Job_Data : Job_Data_Array_Access;

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array);
   procedure Job_Setup (Context : App_Job_Context);
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);
   procedure Job_Post_Process (Context : App_Job_Context);
   procedure App_Post_Process
     (Context : App_Context; Jobs : App_Job_Context_Array);

   package App is new Libadalang.Helpers.App
     (Name               => "nameres",
      Description        =>
         "Run Libadalang's name resolution on a file, set of files or project",
      Enable_Parallelism => True,
      App_Setup          => App_Setup,
      Job_Setup          => Job_Setup,
      Process_Unit       => Process_Unit,
      Job_Post_Process   => Job_Post_Process,
      App_Post_Process   => App_Post_Process);

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
   function "+" (S : Unbounded_Text_Type) return Text_Type
      renames To_Wide_Wide_String;

   function "<" (Left, Right : Ada_Node) return Boolean is
     (Sloc_Range (Left).Start_Line < Sloc_Range (Right).Start_Line);

   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Ada_Node,
      Array_Type   => Ada_Node_Array,
      "<"          => "<");

   function Decode_Boolean_Literal (T : Text_Type) return Boolean is
     (Boolean'Wide_Wide_Value (T));
   procedure Process_File
     (Job_Data : in out Job_Data_Record;
      Unit     : Analysis_Unit;
      Filename : String);

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
   --  Dump the exception ``E``, honoring the ``Args.No_Traceback`` flag (e.g.
   --  don't show tracebacks when asked not to). If ``Obj`` is passed and
   --  ``Args.JSON`` is set, also set fields in ``Obj``.

   procedure Increment (Counter : in out Natural);

   type Supported_Pragma is
     (Ignored_Pragma, Error_In_Pragma, Pragma_Config, Pragma_Section,
      Pragma_Test, Pragma_Test_Statement, Pragma_Test_Statement_UID,
      Pragma_Test_Block, Pragma_Find_All_References);
   type Decoded_Pragma (Kind : Supported_Pragma) is record
      case Kind is
         when Ignored_Pragma =>
            --  Nameres does not handle this pragma
            null;

         when Error_In_Pragma =>
            --  We had trouble decoding this pragma
            Error_Sloc    : Source_Location;
            Error_Message : Unbounded_String;

         when Pragma_Config =>
            --  Tune nameres' settings
            Config_Name : Unbounded_Text_Type;
            Config_Expr : Expr;

         when Pragma_Section =>
            --  Output headlines
            Section_Name : Unbounded_Text_Type;

         when Pragma_Test =>
            --  Run name resolution on the given expression
            Test_Expr  : Expr;
            Test_Debug : Boolean;

         when Pragma_Test_Statement
            | Pragma_Test_Statement_UID
            | Pragma_Test_Block
         =>
            --  Run name resolution on the statement that precedes this pragma.
            --
            --  For the UID version, but show the unique_identifying name of
            --  the declarations instead of the node image.  This is used in
            --  case the node might change (for example in tests where we
            --  resolve runtime things).
            --
            --  For the block version, run name resolution on all xref entry
            --  points in the statement that precedes this pragma, or on the
            --  whole compilation unit if top-level.
            Test_Target : Ada_Node;

         when Pragma_Find_All_References =>
            --  Run the find-all-references property designated by Refs_Kind on
            --  Refs_Target, for all loaded units. If Refs_Target is null, run
            --  the find-all-reference property on all applicable nodes in this
            --  unit.
            Refs_Kind               : Reference_Kind;
            Refs_Target             : Basic_Decl;
            Refs_Imprecise_Fallback : Boolean;
            Refs_Show_Slocs         : Boolean;
      end case;
   end record;

   function Decode_Pragma (Node : Pragma_Node) return Decoded_Pragma;

   -----------
   -- Merge --
   -----------

   procedure Merge (Stats : in out Stats_Record; Other : Stats_Record) is
   begin
      Stats.Nb_Files_Analyzed :=
         Stats.Nb_Files_Analyzed + Other.Nb_Files_Analyzed;
      Stats.Nb_Successes := Stats.Nb_Successes + Other.Nb_Successes;
      Stats.Nb_Fails := Stats.Nb_Fails + Other.Nb_Fails;
      Stats.Nb_Xfails := Stats.Nb_Xfails + Other.Nb_Xfails;
      Stats.Nb_Exception_Fails :=
         Stats.Nb_Exception_Fails + Other.Nb_Exception_Fails;

      if Stats.Max_Nb_Fails < Other.Max_Nb_Fails then
         Stats.Max_Nb_Fails := Other.Max_Nb_Fails;
         Stats.File_With_Most_Fails := Other.File_With_Most_Fails;
      end if;
   end Merge;

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
   -- Increment --
   ---------------

   procedure Increment (Counter : in out Natural) is
   begin
      Counter := Counter + 1;
   end Increment;

   -------------------
   -- Decode_Pragma --
   -------------------

   function Decode_Pragma (Node : Pragma_Node) return Decoded_Pragma is

      Name         : constant String := Text (Node.F_Id);
      Untyped_Args : constant Ada_Node_Array := Node.F_Args.Children;
      Args         : array (Untyped_Args'Range) of Pragma_Argument_Assoc;

      function Error (Message : String) return Decoded_Pragma
      is ((Error_In_Pragma, Start_Sloc (Node.Sloc_Range), +Message));
      --  Shortcut to build Error_In_Pragma records

      function N_Args_Error (Expected : Natural) return Decoded_Pragma
      is (Error ("expected " & Expected'Image & " pragma arguments, got"
                 & Args'Length'Image));
      function N_Args_Error
        (At_Least, At_Most : Natural) return Decoded_Pragma
      is (Error ("expected between" & At_Least'Image & " and" & At_Most'Image
                 & " pragma arguments, got" & Args'Length'Image));
      --  Return an Error_In_Pragma record for an unexpected number of pragma
      --  arguments.

   begin
      for I in Untyped_Args'Range loop
         Args (I) := Untyped_Args (I).As_Pragma_Argument_Assoc;
      end loop;

      if Name = "Config" then
         if Args'Length /= 1 then
            return N_Args_Error (1);
         elsif Args (1).F_Id.Is_Null then
            return Error ("Missing argument name");
         elsif Args (1).F_Id.Kind /= Ada_Identifier then
            return Error ("Argument name must be an identifier");
         else
            return (Pragma_Config,
                    To_Unbounded_Text (Args (1).F_Id.Text),
                    Args (1).F_Expr);
         end if;

      elsif Name = "Section" then
         if Args'Length /= 1 then
            return N_Args_Error (1);
         elsif not Args (1).F_Id.Is_Null then
            return Error ("No argument name allowed");
         elsif Args (1).F_Expr.Kind /= Ada_String_Literal then
            return Error ("Section name must be a string literal");
         else
            return (Pragma_Section,
                    To_Unbounded_Text (Args (1).F_Expr
                    .As_String_Literal.P_Denoted_Value));
         end if;

      elsif Name = "Test" then
         if Args'Length not in 1 | 2 then
            return N_Args_Error (1, 2);
         end if;

         declare
            Result : Decoded_Pragma (Pragma_Test);
         begin
            if not Args (1).F_Id.Is_Null then
               return Error ("No argument name allowed");
            end if;
            Result.Test_Expr := Args (1).F_Expr;

            if Args'Length > 1 then
               if not Args (2).F_Id.Is_Null then
                  return Error ("No argument name allowed");
               elsif Args (2).F_Expr.Kind /= Ada_Identifier
                     or else Args (2).F_Expr.Text /= "Debug"
               then
                  return Error ("When present, the second argument must be the"
                                & "Debug identifier");
               end if;
               Result.Test_Debug := True;
            else
               Result.Test_Debug := False;
            end if;

            return Result;
         end;

      elsif Name = "Test_Statement" then
         if Args'Length /= 0 then
            return N_Args_Error (0);
         end if;
         return (Pragma_Test_Statement, Node.Previous_Sibling);

      elsif Name = "Test_Statement_UID" then
         if Args'Length /= 0 then
            return N_Args_Error (0);
         end if;
         return (Pragma_Test_Statement_UID, Node.Previous_Sibling);

      elsif Name = "Test_Block" then
         if Args'Length /= 0 then
            return N_Args_Error (0);
         end if;
         declare
            Parent : constant Ada_Node := Node.Parent.Parent;
            Target : constant Ada_Node :=
              (if Parent.Kind = Ada_Compilation_Unit
               then Parent.As_Compilation_Unit.F_Body
               else Node.Previous_Sibling);
         begin
            return (Pragma_Test_Block, Target);
         end;

      elsif Name = "Find_All_References" then
         if Args'Length not in 1 .. 4 then
            return N_Args_Error (1, 4);
         end if;

         declare
            Result : Decoded_Pragma (Pragma_Find_All_References);

            N : Positive := 1;
            --  Logical index of the next positional argument to process

            Target : Ada_Node := Node.Previous_Sibling;
            --  Temporary to compute the find-all-refs target. By default,
            --  target the declaration that precedes the pragma.

            Imprecise_Fallback : Boolean renames
               Result.Refs_Imprecise_Fallback;
            Show_Slocs         : Boolean renames Result.Refs_Show_Slocs;
         begin
            Imprecise_Fallback := False;
            Show_Slocs := True;

            for A of Args loop
               if A.F_Id.Is_Null then
                  --  This is a positional pragma argument

                  case N is
                  when 1 =>
                     if A.F_Expr.Kind /= Ada_Identifier then
                        return Error ("Identifier expected");
                     end if;

                     declare
                        Name : constant Text_Type := A.F_Expr.Text;
                     begin
                        --  The first argument is the reference kind
                        if Name = "Any" then
                           Result.Refs_Kind := Any;
                        elsif Name = "Calls" then
                           Result.Refs_Kind := Subp_Call;
                        elsif Name = "Overrides" then
                           Result.Refs_Kind := Subp_Overriding;
                        elsif Name = "Derived" then
                           Result.Refs_Kind := Type_Derivation;
                        else
                           return Error
                             ("Invalid reference kind: " & Image (Name));
                        end if;
                     end;

                  when 2 =>
                     --  The optional second argument is the find-all-refs
                     --  target.
                     case A.F_Expr.Kind is
                     when Ada_Identifier =>
                        --  Raw identifier: directive to locate the target
                        declare
                           Name : constant Text_Type := A.F_Expr.Text;
                        begin
                           if Name = "All_Decls" then
                              Target := No_Ada_Node;

                           elsif Name = "Previous_Decl" then
                              null;

                           elsif Name = "Previous_Referenced_Decl" then
                              --  Assume that the previous statement is a call:
                              --  the target is the callee.
                              Target := Target.As_Call_Stmt.F_Call.As_Ada_Node;
                              if Target.Kind = Ada_Call_Expr then
                                 Target := Target.As_Call_Expr.F_Name
                                      .P_Referenced_Decl.As_Ada_Node;
                              end if;

                           else
                              return Error ("Invalid target: " & Image (Name));
                           end if;
                        end;

                     when Ada_String_Literal =>
                        --  String literal: name of the target. Look for the
                        --  last declaration with this name that appears in
                        --  the source before this pragma.
                        declare
                           Name : constant Text_Type :=
                              A.F_Expr.As_String_Literal.P_Denoted_Value;
                           It   : Traverse_Iterator'Class := Find
                             (Node.Unit.Root, Kind_In (Ada_Basic_Decl'First,
                                                       Ada_Basic_Decl'Last));
                           N    : Ada_Node;

                           Pragma_Sloc : constant Source_Location :=
                              Start_Sloc (Node.Sloc_Range);
                        begin
                           Target := No_Ada_Node;
                           Decl_Loop : while It.Next (N)
                              and Start_Sloc (N.Sloc_Range) < Pragma_Sloc
                           loop
                              for DN of N.As_Basic_Decl.P_Defining_Names loop
                                 if DN.Text = Name then
                                    Target := N;
                                    exit Decl_Loop;
                                 end if;
                              end loop;
                           end loop Decl_Loop;

                           if Target.Is_Null then
                              return Error
                                ("No declaration for " & Image (Name));
                           end if;
                        end;

                     when others =>
                        return Error ("Unexpected expression (identifier or"
                                      & " string literal expected)");
                     end case;

                  when others =>
                     return Error ("Too many positional arguments");
                  end case;
                  N := N + 1;

               else
                  --  This is a named argument. The grammar should make sure
                  --  that names for pragma arguments are identifiers.
                  declare
                     pragma Assert (A.F_Id.Kind = Ada_Identifier);
                     Name : constant Text_Type := A.F_Id.Text;
                  begin
                     if Name = "Imprecise_Fallback" then
                        Imprecise_Fallback := Decode_Boolean_Literal
                          (A.F_Expr.Text);
                     elsif Name = "Show_Slocs" then
                        Show_Slocs := Decode_Boolean_Literal
                          (A.F_Expr.Text);
                     else
                        return Error ("Unknown argument: " & Image (Name));
                     end if;
                  end;
               end if;
            end loop;

            --  Make sure we received at least one positional argument

            if N = 1 then
               return Error ("Missing first positional argument");
            end if;

            Result.Refs_Target := Target.As_Basic_Decl;
            return Result;
         end;

      else
         return (Kind => Ignored_Pragma);
      end if;
   end Decode_Pragma;

   ---------------
   -- App_Setup --
   ---------------

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array) is
      pragma Unreferenced (Context);
   begin
      if Args.No_Lookup_Cache.Get then
         Disable_Lookup_Cache (True);
      end if;

      if Args.Trace.Get then
         Set_Debug_State (Trace);
      elsif Args.Debug.Get then
         Set_Debug_State (Step);
      end if;

      Job_Data := new Job_Data_Array'(Jobs'Range => (others => <>));
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
      Job_Data : Job_Data_Record renames Nameres.Job_Data (Context.ID);
      Basename : constant String := +Create (+Unit.Get_Filename).Base_Name;

      Before, After : Time;
      Time_Elapsed  : Duration;
   begin
      if not Quiet then
         Put_Title ('#', "Analyzing " & Basename);
      end if;
      Before := Clock;

      begin
         Process_File (Job_Data, Unit, Unit.Get_Filename);
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

      Increment (Job_Data.Stats.Nb_Files_Analyzed);
      if Args.File_Limit.Get /= -1
         and then Job_Data.Stats.Nb_Files_Analyzed
                  >= Args.File_Limit.Get
      then
         Abort_App ("Requested file limit reached: aborting");
      end if;
   end Process_Unit;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File
     (Job_Data : in out Job_Data_Record;
      Unit     : Analysis_Unit;
      Filename : String)
   is
      Nb_File_Fails : Natural := 0;
      --  Number of name resolution failures not covered by XFAILs we had in
      --  this file.

      Config : Config_Record renames Job_Data.Config;

      Empty : Boolean := True;
      --  False if processing pragmas has output at least one line. True
      --  otherwise.

      function Is_Pragma_Node (N : Ada_Node) return Boolean is
        (Kind (N) = Ada_Pragma_Node);

      procedure Process_Pragma (Node : Ada_Node);
      --  Decode a pragma node and run actions accordingly (trigger name
      --  resolution, output a section name, ...).

      procedure Resolve_Node (Node : Ada_Node; Show_Slocs : Boolean := True);
      --  Run name resolution testing on Node.
      --
      --  This involves running P_Resolve_Names on Node, displaying resolved
      --  references, updating statistics, creating a JSON report if requested,
      --  etc.

      function Is_Xref_Entry_Point (N : Ada_Node) return Boolean
      is (P_Xref_Entry_Point (N)
          and then
            (Args.Solve_Line.Get = 0
             or else
             Natural (Sloc_Range (N).Start_Line) = Args.Solve_Line.Get));
      --  Return whether we should use N as an entry point for name resolution
      --  testing.

      procedure Resolve_Block (Block : Ada_Node);
      --  Call Resolve_Node on all xref entry points (according to
      --  Is_Xref_Entry_Point) in Block except for Block itself.

      --------------------
      -- Process_Pragma --
      --------------------

      procedure Process_Pragma (Node : Ada_Node) is
         P : constant Decoded_Pragma := Decode_Pragma (Node.As_Pragma_Node);
      begin
         case P.Kind is
         when Ignored_Pragma =>
            null;

         when Error_In_Pragma =>
            Put_Line (Image (P.Error_Sloc) & ": " & (+P.Error_Message));
            Empty := False;

         when Pragma_Config =>
            declare
               Value : constant Text_Type := Text (P.Config_Expr);
            begin
               if P.Config_Name = "Display_Slocs" then
                  Config.Display_Slocs := Decode_Boolean_Literal (Value);
               elsif P.Config_Name = "Display_Images" then
                  Config.Display_Images :=
                     Decode_Boolean_Literal (Value);
               else
                  raise Program_Error with
                     "Invalid configuration: " & Image (+P.Config_Name);
               end if;
            end;

         when Pragma_Section =>
            if not Quiet then
               Put_Title ('-', Image (To_Wide_Wide_String (P.Section_Name)));
            end if;
            Empty := True;

         when Pragma_Test =>
            Trigger_Envs_Debug (P.Test_Debug);

            declare
               Entities : Ada_Node_Array := Do_Pragma_Test (P.Test_Expr);
            begin
               Put_Line (Text (P.Test_Expr) & " resolves to:");
               Sort (Entities);
               for E of Entities loop
                  Put ("    " & (if Config.Display_Images
                                 then E.Image
                                 else E.Debug_Text));
                  if Config.Display_Slocs then
                     Put_Line (" at " & Image (Start_Sloc (E.Sloc_Range)));
                  else
                     New_Line;
                  end if;
               end loop;
               if Entities'Length = 0 then
                  Put_Line ("    <none>");
               end if;
            end;

            Empty := False;
            Trigger_Envs_Debug (False);

         when Pragma_Test_Statement | Pragma_Test_Statement_UID =>
            Resolve_Node (Node       => P.Test_Target,
                          Show_Slocs => P.Kind /= Pragma_Test_Statement_UID);
            Empty := False;

         when Pragma_Test_Block =>
            Resolve_Block (P.Test_Target);
            Empty := False;

         when Pragma_Find_All_References =>
            Job_Data.Refs_Requests.Append
              ((P.Refs_Kind,
                P.Refs_Target,
                P.Refs_Imprecise_Fallback,
                P.Refs_Show_Slocs,
                Node.As_Pragma_Node));
         end case;
      end Process_Pragma;

      -------------------
      -- Resolve_Block --
      -------------------

      procedure Resolve_Block (Block : Ada_Node) is

         procedure Resolve_Entry_Point (Node : Ada_Node);
         --  Callback for tree traversal in Block

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
         --  Callback for the tree traversal in Node. Print xref info for N.

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
               Put_Line ("Expr: " & N.Image);
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
                        (if Show_Slocs or else Decl_Name.Is_Null
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
                          with "Invalid arg for " & N.Image;
                     end if;
                     Put_Line ("XFAIL: " & Image (Arg.P_Denoted_Value, False));
                     Put_Line ("");
                  end;
               end if;
               return True;
            end if;
            return False;
         end XFAIL;

         Verbose     : constant Boolean :=
            not (Quiet or else Args.Only_Show_Failures.Get);
         Output_JSON : constant Boolean := Args.JSON.Get;

         Dummy : Visit_Status;
         Obj   : aliased J.JSON_Value;

      begin
         --  Pre-processing output

         if Output_JSON then
            Obj := J.Create_Object;
            Obj.Set_Field ("kind", "node_resolution");
            Obj.Set_Field ("file", Filename);
            Obj.Set_Field ("sloc", Image (Node.Sloc_Range));
         end if;
         if Verbose then
            Put_Title ('*', "Resolving xrefs for node " & Node.Image);
         end if;
         if Langkit_Support.Adalog.Debug.Debug then
            Assign_Names_To_Logic_Vars (Node);
         end if;

         --  Perform name resolution

         if P_Resolve_Names (Node) or else Args.Imprecise_Fallback.Get then
            if not Args.Only_Show_Failures.Get then
               Dummy := Traverse (Node, Print_Node'Access);
            end if;

            Increment (Job_Data.Stats.Nb_Successes);

            if Output_JSON then
               Obj.Set_Field ("success", True);
            end if;
         else
            Put_Line ("Resolution failed for node " & Node.Image);
            if XFAIL then
               Increment (Job_Data.Stats.Nb_Xfails);
            else
               Increment (Nb_File_Fails);
            end if;

            if Output_JSON then
               Obj.Set_Field ("success", False);
            end if;
         end if;

         --  Post-processing output

         if Verbose then
            Put_Line ("");
         end if;
         if Output_JSON then
            Ada.Text_IO.Put_Line (Obj.Write);
         end if;
      exception
         when E : others =>
            Put_Line
              ("Resolution failed with exception for node " & Node.Image);
            Dump_Exception (E, Obj);

            if XFAIL then
               Increment (Job_Data.Stats.Nb_Xfails);
            else
               Increment (Job_Data.Stats.Nb_Exception_Fails);
               Increment (Nb_File_Fails);
            end if;
      end Resolve_Node;

   begin
      --  Make sure there is no diagnostic for this unit. If there are, print
      --  them and do nothing else.

      if Has_Diagnostics (Unit) then
         for D of Diagnostics (Unit) loop
            Put_Line (Format_GNU_Diagnostic (Unit, D));
         end loop;
         return;
      end if;

      --  Manually trigger PLE first, and if requested reparse the unit, to
      --  make sure that rebuilding lexical envs works correctly.

      Populate_Lexical_Env (Unit);
      if Args.Do_Reparse.Get then
         for I in 1 .. 10 loop
            Reparse (Unit);
            Populate_Lexical_Env (Unit);
         end loop;
      end if;

      Job_Data.Config := (others => <>);

      if Args.Dump_Envs.Get then
         Put_Title ('-', "Dumping envs for " & Filename);
         Dump_Lexical_Env (Unit);
      end if;

      if Args.Resolve_All.Get or Args.Solve_Line.Get /= 0 then
         Resolve_Block (Root (Unit));
      end if;

      --  Run through all pragmas and execute the associated actions

      declare
         It : Traverse_Iterator'Class :=
            Find (Unit.Root, Is_Pragma_Node'Access);
      begin
         It.Iterate (Process_Pragma'Access);
      end;
      if not Empty then
         New_Line;
      end if;

      --  Update statistics

      Job_Data.Stats.Nb_Fails := Job_Data.Stats.Nb_Fails + Nb_File_Fails;
      if Job_Data.Stats.Max_Nb_Fails < Nb_File_Fails then
         Job_Data.Stats.File_With_Most_Fails := +Filename;
         Job_Data.Stats.Max_Nb_Fails := Nb_File_Fails;
      end if;

   end Process_File;

   ----------------------
   -- Job_Post_Process --
   ----------------------

   procedure Job_Post_Process (Context : App_Job_Context) is
      Job_Data : Job_Data_Record renames Nameres.Job_Data (Context.ID);
      Units    : Analysis_Unit_Array
        (1 .. Natural (Context.Units_Processed.Length));

      function Is_Refs_Target
        (Decl : Basic_Decl;
         Kind : Reference_Kind) return Boolean
      is (case Kind is
          when Any                         => True,
          when Subp_Call | Subp_Overriding => Decl.P_Is_Subprogram,
          when Type_Derivation             => Decl.Kind = Ada_Type_Decl);
      --  Return whether Decl is a valid target for the given Kind request

      procedure Process_Refs
        (Target             : Basic_Decl;
         Kind               : Reference_Kind;
         Imprecise_Fallback : Boolean;
         Show_Slocs         : Boolean);
      --  Call the find-all-reference property corresponding to Kind on Target

      ------------------
      -- Process_Refs --
      ------------------

      procedure Process_Refs
        (Target             : Basic_Decl;
         Kind               : Reference_Kind;
         Imprecise_Fallback : Boolean;
         Show_Slocs         : Boolean)
      is
         Empty : Boolean := True;
         What  : constant String :=
           (case Kind is
            when Any             => "References to ",
            when Subp_Call       => "Calls to ",
            when Subp_Overriding => "Overridings for ",
            when Type_Derivation => "Derivations for ");

         function Locator (Node : Ada_Node'Class) return String;
         --  Return a location string for Node suitable for test output

         procedure Print_Ref (Ref : Ada_Node'Class);

         -------------
         -- Locator --
         -------------

         function Locator (Node : Ada_Node'Class) return String is
            File : constant String :=
               +Create (+Node.Unit.Get_Filename).Base_Name;
            Sloc : constant String := Image (Node.Sloc_Range);
         begin
            return
               "(" & File & (if Show_Slocs
                             then ", " & Sloc
                             else "") & ")";
         end Locator;

         ---------------
         -- Print_Ref --
         ---------------

         procedure Print_Ref (Ref : Ada_Node'Class) is
            Ref_Decl : Ada_Node := Ref.As_Ada_Node;
         begin
            --  Follow Ref's parents chain until we have an xref entry point,
            --  to get meaningful references.
            while not Ref_Decl.Is_Null
                  and then not Ref_Decl.Parent.Is_Null
                  and then not Ref_Decl.P_Xref_Entry_Point
            loop
               Ref_Decl := Ref_Decl.Parent;
            end loop;

            declare
               Text      : constant Text_Type := Ref_Decl.Text;
               Text_Last : Natural := Text'Last;
            begin
               --  Look for the first line break in Text
               for I in Text'Range loop
                  if Text (I) in Chars.CR | Chars.LF then
                     Text_Last := I - 1;
                     exit;
                  end if;
               end loop;

               Put_Line ("   " & Image (Text (Text'First .. Text_Last)) & " "
                         & Locator (Ref_Decl));
            end;

            Empty := False;
         end Print_Ref;

      begin
         for DN of Target.P_Defining_Names loop
            Put_Line (What & DN.Debug_Text & " " & Locator (DN));
            case Kind is
            when Any =>
               for R of DN.P_Find_All_References (Units, Imprecise_Fallback)
               loop
                  Print_Ref (Ref (R));
               end loop;

            when Subp_Call =>
               for R of DN.P_Find_All_Calls (Units, Imprecise_Fallback)
               loop
                  Print_Ref (Ref (R));
               end loop;

            when Subp_Overriding =>
               for R of DN.P_Semantic_Parent.As_Basic_Decl.P_Find_All_Overrides
                 (Units, Imprecise_Fallback)
               loop
                  Print_Ref (R);
               end loop;

            when Type_Derivation =>
               for R of DN.P_Semantic_Parent.As_Type_Decl
                        .P_Find_All_Derived_Types (Units, Imprecise_Fallback)
               loop
                  Print_Ref (R);
               end loop;
            end case;
         end loop;

         if Empty then
            Put_Line ("   <none>");
         end if;
      end Process_Refs;

   begin
      --  Compute the list of units

      for I in Units'Range loop
         Units (I) := Context.Units_Processed (I);
      end loop;

      --  Process all Find_All_References requests

      for R of Job_Data.Refs_Requests loop
         if R.Target.Is_Null then
            --  If there is no target, run the request on all valid targets in
            --  the unit that contains the pragma.
            declare
               It     : Traverse_Iterator'Class := Find
                 (R.From_Pragma.Unit.Root,
                  Kind_In (Ada_Basic_Decl'First, Ada_Basic_Decl'Last));
               Target : Ada_Node;
               T      : Basic_Decl;
            begin
               while It.Next (Target) loop
                  T := Target.As_Basic_Decl;
                  if Is_Refs_Target (T, R.Kind) then
                     Process_Refs
                       (T, R.Kind, R.Imprecise_Fallback, R.Show_Slocs);
                  end if;
               end loop;
            end;

         else
            Process_Refs
              (R.Target, R.Kind, R.Imprecise_Fallback, R.Show_Slocs);
         end if;
      end loop;
   end Job_Post_Process;

   ----------------------
   -- App_Post_Process --
   ----------------------

   procedure App_Post_Process
     (Context : App_Context; Jobs : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      Stats : Stats_Record;
      Total : Natural;
   begin
      --  Aggregate statistics from all jobs

      Stats := Job_Data (Jobs'First).Stats;
      for I in Jobs'First + 1 .. Jobs'Last loop
         Merge (Stats, Job_Data (I).Stats);
      end loop;
      Total := Stats.Nb_Successes + Stats.Nb_Fails + Stats.Nb_Xfails;

      if Args.Stats.Get and then Total > 0 then
         declare
            type Percentage is delta 0.01 range 0.0 .. 0.01 * 2.0**32;
            Percent_Successes : constant Percentage := Percentage
              (Float (Stats.Nb_Successes) / Float (Total) * 100.0);
            Percent_Failures : constant Percentage :=
              Percentage (Float (Stats.Nb_Fails) / Float (Total) * 100.0);

            Percent_XFAIL : constant Percentage :=
              Percentage (Float (Stats.Nb_Xfails) / Float (Total) * 100.0);
         begin
            Put_Line ("Resolved " & Total'Image & " nodes");
            Put_Line ("Of which" & Stats.Nb_Successes'Image
                      & " successes - " & Percent_Successes'Image & "%");
            Put_Line ("Of which" & Stats.Nb_Fails'Image
                      & " failures - " & Percent_Failures'Image & "%");

            Put_Line ("Of which" & Stats.Nb_Xfails'Image
                      & " XFAILS - " & Percent_XFAIL'Image & "%");
            Put_Line ("File with most failures ("
                      & Stats.Max_Nb_Fails'Image
                      & "):" & (+Stats.File_With_Most_Fails));
         end;
      end if;

      Free (Job_Data);

      Put_Line ("Done.");
   end App_Post_Process;

begin
   App.Run;
end Nameres;
