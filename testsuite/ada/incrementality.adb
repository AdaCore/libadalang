with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;            use Ada.Text_IO;

with GNATCOLL.Opt_Parse;

with Langkit_Support.Hashes; use Langkit_Support.Hashes;
with Langkit_Support.Slocs;  use Langkit_Support.Slocs;

with Libadalang.Analysis;    use Libadalang.Analysis;
with Libadalang.Common;      use Libadalang.Common;
with Libadalang.Helpers;     use Libadalang.Helpers;
with Libadalang.Iterators;   use Libadalang.Iterators;

procedure Incrementality is
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);
   --  Compute baseline name resolution results for this unit

   procedure App_Post_Process
     (Context : App_Context; Jobs : App_Job_Context_Array);
   --  Perform the actual stress testing: apply modifications to the sources,
   --  re-run name resolution, undo the modifications and compare the results
   --  to the baseline.

   function All_Processed_Units
     (Jobs : App_Job_Context_Array) return Analysis_Unit_Array;
   --  Return the concatenation of all analysis units processed by each job

   type Stage_Kind is (Build_DB, Reset_Caches, Compare_DB);
   --  Processing each node is done in three stages

   procedure Resolve_Unit (Unit : Analysis_Unit; Stage : Stage_Kind);
   --  Compute baseline name resolution results for this unit

   function Is_Xref_Entry_Point (N : Ada_Node) return Boolean
   is (P_Xref_Entry_Point (N));
   --  Return whether we should use N as an entry point for name resolution
   --  testing.

   package App is new Libadalang.Helpers.App
     (Name                   => "incrementality",
      Description            =>
         "Stress test incrementality of name resolution",
      Enable_Parallelism     => False,
      GPR_Absent_Dir_Warning => False,
      Process_Unit           => Process_Unit,
      App_Post_Process       => App_Post_Process);

   package Args is
      use GNATCOLL.Opt_Parse;

      package Verbose is new Parse_Flag
        (App.Args.Parser, "-v", "--verbose",
         Help => "Log additional information about the run");
   end Args;

   type Node_Location is record
      Filename : Unbounded_String;
      Sloc     : Source_Location_Range;
   end record;

   No_Node_Location : constant Node_Location :=
     (Null_Unbounded_String, No_Source_Location_Range);

   function Hash (Loc : Node_Location) return Ada.Containers.Hash_Type is
     (Combine ((Ada.Strings.Unbounded.Hash (Loc.Filename),
                Ada.Containers.Hash_Type (Loc.Sloc.Start_Line),
                Ada.Containers.Hash_Type (Loc.Sloc.End_Line),
                Ada.Containers.Hash_Type (Loc.Sloc.Start_Column),
                Ada.Containers.Hash_Type (Loc.Sloc.End_Column))));

   function Lookup (Loc : Node_Location) return Ada_Node;
   --  Return a string representation for the given node location

   function Get_Location (Node : Ada_Node) return Node_Location;
   --  Compute the location for the given node

   package Node_Results is new Ada.Containers.Hashed_Maps
     (Node_Location, Node_Location, Hash, "=", "=");

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);
   begin
      if Args.Verbose.Get then
         Put_Line ("Populating DB for " & Unit.Get_Filename);
      end if;
      Resolve_Unit (Unit, Build_DB);
   end Process_Unit;

   Lookup_Ctx : constant Analysis_Context := Create_Context;
   Nameres_DB : Node_Results.Map;

   ------------
   -- Lookup --
   ------------

   function Lookup (Loc : Node_Location) return Ada_Node is
      Unit : constant Analysis_Unit :=
         Lookup_Ctx.Get_From_File (To_String (Loc.Filename));
   begin
      return Unit.Root.Lookup ((Loc.Sloc.Start_Line, Loc.Sloc.Start_Column));
   end Lookup;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Node : Ada_Node) return Node_Location is
   begin
      if Node.Is_Null then
         return No_Node_Location;
      else
         return
           (Filename => To_Unbounded_String (Node.Unit.Get_Filename),
            Sloc     => Node.Sloc_Range);
      end if;
   end Get_Location;

   ------------------
   -- Resolve_Unit --
   ------------------

   procedure Resolve_Unit (Unit : Analysis_Unit; Stage : Stage_Kind) is

      procedure Resolve_Entry_Point (Node : Ada_Node);
      --  Callback for tree traversal in Block

      function Resolve_Node
        (Node : Ada_Node'Class; Result : out Ada_Node) return Boolean;
      --  If the given node is subject to cross-references, return True and
      --  store the cross-reference result in ``Result``. For a reference,
      --  this corresponds to the referenced defining name. For an expression,
      --  this corresponds to its type.

      function Populate_DB (Node : Ada_Node'Class) return Visit_Status;
      --  Populate ``Nameres_DB`` with cross-reference data of all relevant
      --  nodes inside the given ``Node``.

      function Compare_DB (Node : Ada_Node'Class) return Visit_Status;
      --  Compare the cross-reference data of all the nodes inside the given
      --  ``Node`` with those stored ``Nameres_DB``. Report any mismatch.

      -------------------------
      -- Resolve_Entry_Point --
      -------------------------

      procedure Resolve_Entry_Point (Node : Ada_Node) is
         Dummy  : Visit_Status;
      begin
         case Stage is
            when Build_DB =>
               Dummy := Traverse (Node, Populate_DB'Access);
            when Reset_Caches =>
               declare
                  Dummy : Boolean;
               begin
                  Dummy := Node.P_Resolve_Names;
               exception
                  when Property_Error =>
                     null;
               end;
            when Compare_DB =>
               Dummy := Traverse (Node, Compare_DB'Access);
         end case;
      end Resolve_Entry_Point;

      ------------------
      -- Resolve_Node --
      ------------------

      function Resolve_Node
        (Node : Ada_Node'Class; Result : out Ada_Node) return Boolean
      is
      begin
         if Node.Kind in Ada_Name then
            if not Node.As_Name.P_Is_Defining then
               Result := Node.As_Name.P_Referenced_Defining_Name.As_Ada_Node;
               return True;
            end if;
         elsif Node.Kind in Ada_Expr then
            Result := Node.As_Expr.P_Expression_Type.As_Ada_Node;
            return True;
         end if;
         return False;
      exception
         when Property_Error =>
            Result := No_Ada_Node;
            return True;
      end Resolve_Node;

      -----------------
      -- Populate_DB --
      -----------------

      function Populate_DB (Node : Ada_Node'Class) return Visit_Status is
         Resolved : Ada_Node;
      begin
         if not Resolve_Node (Node, Resolved) then
            return Into;
         end if;

         Nameres_DB.Include
           (Get_Location (Node.As_Ada_Node),
            Get_Location (Resolved));

         return Into;
      end Populate_DB;

      ----------------
      -- Compare_DB --
      ----------------

      function Compare_DB (Node : Ada_Node'Class) return Visit_Status is
         Resolved : Ada_Node;
      begin
         if not Resolve_Node (Node, Resolved) then
            return Into;
         end if;

         declare
            Node_Loc     : constant Node_Location :=
              Get_Location (Node.As_Ada_Node);
            Resolved_Loc : constant Node_Location :=
              Get_Location (Resolved);
            C : constant Node_Results.Cursor :=
               Nameres_DB.Find (Node_Loc);
         begin
            if Node_Results.Has_Element (C) then
               if Resolved_Loc /= Node_Results.Element (C) then
                  Put_Line ("Inconsistent result at " & Node.Image);
                  Put_Line ("  Previous was : "
                            & Lookup (Node_Results.Element (C)).Image);
                  Put_Line ("  Current is : " & Resolved.Image);
               end if;
            else
               raise Program_Error with "Found new entry";
            end if;
         end;

         return Into;
      end Compare_DB;

      It : Ada_Node_Iterators.Iterator'Class :=
         Find (Unit.Root, Is_Xref_Entry_Point'Access);
   begin
      It.Iterate (Resolve_Entry_Point'Access);
   end Resolve_Unit;

   -------------------------
   -- All_Processed_Units --
   -------------------------

   function All_Processed_Units
     (Jobs : App_Job_Context_Array) return Analysis_Unit_Array
   is
      All_Units : Unit_Vectors.Vector;
      I         : Natural := 1;
   begin
      for J of Jobs loop
         All_Units.Insert (All_Units.Last, J.Units_Processed);
      end loop;
      return Result : Analysis_Unit_Array (1 .. Natural (All_Units.Length)) do
         for U of All_Units loop
            Result (I) := U;
            I := I + 1;
         end loop;
      end return;
   end All_Processed_Units;

   ----------------------
   -- App_Post_Process --
   ----------------------

   procedure App_Post_Process
     (Context : App_Context; Jobs : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      All_Units : constant Analysis_Unit_Array := All_Processed_Units (Jobs);
      Count     : Natural := 1;
   begin
      if Args.Verbose.Get then
         Put_Line ("Populated DB with" & Nameres_DB.Length'Image & " items.");
      end if;

      while Count <= All_Units'Length loop
         declare
            Impacted : constant Analysis_Unit_Array :=
               All_Units (Count).Root.P_Filter_Is_Imported_By
                 (All_Units, Transitive => True);
         begin
            if Args.Verbose.Get then
               Put_Line ("Clearing " & All_Units (Count).Get_Filename);
               for U of Impacted loop
                  Put_Line ("  Impacts " & U.Get_Filename);
               end loop;
            end if;

            All_Units (Count).Reparse (Buffer => "");

            for U of Impacted loop
               Resolve_Unit (U, Reset_Caches);
            end loop;

            All_Units (Count).Reparse;

            for U of Impacted loop
               Resolve_Unit (U, Compare_DB);
            end loop;
         end;
         Count := Count + 1;
      end loop;

      Put_Line ("Done.");
   end App_Post_Process;
begin
   App.Run;
end Incrementality;
