with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.VFS;    use GNATCOLL.VFS;

with Langkit_Support.Diagnostics;
with Langkit_Support.Diagnostics.Output;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Helpers;   use Libadalang.Helpers;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Check_Ambiguity is

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);

   function Is_Xref_Entry_Point (N : Ada_Node) return Boolean
   is (P_Xref_Entry_Point (N));

   package App is new Libadalang.Helpers.App
     (Name                   => "check_ambiguity",
      Description            =>
         "Check name resolution ambiguity for all xref entry points",
      Enable_Parallelism     => False,
      GPR_Absent_Dir_Warning => False,
      Process_Unit           => Process_Unit);

   procedure Check_Node (Node : Ada_Node);
   --  Check one xref entry point and print a diagnostic if ambiguous

   Entry_Point_Count : Natural := 0;
   --  Keep track of the number of entry points checked to report it at the
   --  end of the program. Can be used to ensure that we did run through
   --  the given sources, even when no ambiguities were found.

   ----------------
   -- Check_Node --
   ----------------

   procedure Check_Node (Node : Ada_Node) is
      Ambiguous : Boolean;
   begin
      begin
         Ambiguous := Node.P_Is_Nameres_Ambiguous;
         Entry_Point_Count := Entry_Point_Count + 1;
      exception
         when Property_Error => return;
      end;
      if Ambiguous then
         declare
            Filename    : constant String :=
              +Create (+Node.Unit.Get_Filename).Base_Name;
            In_Standard : constant Boolean := Filename = "__standard";
            Location    : constant Source_Location_Range :=
              (if In_Standard
               then No_Source_Location_Range
               else Node.Sloc_Range);
            Diag : constant Langkit_Support.Diagnostics.Diagnostic :=
              Langkit_Support.Diagnostics.Create
                (Location, "name resolution is ambiguous");
         begin
            Langkit_Support.Diagnostics.Output.Print_Diagnostic
              (Diag,
               Node.Unit,
               (if In_Standard
                then "in Standard"
                else Filename));
         end;
      end if;
   end Check_Node;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);
      It : Ada_Node_Iterators.Iterator'Class :=
         Find (Unit.Root, Is_Xref_Entry_Point'Access);
   begin
      It.Iterate (Check_Node'Access);
   end Process_Unit;

begin
   App.Run;
   Put_Line ("Checked" & Entry_Point_Count'Image & " entry points.");
   Put_Line ("Done.");
end Check_Ambiguity;
