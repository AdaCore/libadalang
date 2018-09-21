with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Text_IO;                use Ada.Text_IO;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context;
   Unit : Analysis_Unit := Get_From_File (Ctx, "foo.adb");

   ----------
   -- Proc --
   ----------

   procedure Proc (N : Ada_Node) is
   begin
      Put_Line ("  " & To_String (Short_Image (N)));
   end Proc;

begin
   declare
      It : Ada_Node_Iterators.Iterator'Class := Traverse (Unit.Root);
   begin
      Put_Line
        ("This is the list of all nodes in foo.adb, in prefix depth-first"
         & " order:");
      Ada_Node_Iterators.Iterate (It, Proc'Access);
   end;

   declare
      It : Ada_Node_Iterators.Iterator'Class := Traverse (No_Ada_Node);
   begin
      Put_Line
        ("Note that we also can iterate on null nodes:");
      Ada_Node_Iterators.Iterate (It, Proc'Access);
   end;

   declare
      --  For convenience, we prefer to keep the predicate in this file. It
      --  does not access non-local variables, so all should be fine.

      It : Ada_Node_Iterators.Iterator'Class :=
        Find (Unit.Root, Kind_Is (Ada_Identifier));
   begin
      Put_Line
        ("Alright, now here's the list of all identifier nodes:");
      Ada_Node_Iterators.Iterate (It, Proc'Access);
   end;

   Put_Line ("Done.");
end Main;
