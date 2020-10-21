with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx   : constant Analysis_Context := Create_Context;
   Unit  : constant Analysis_Unit := Get_From_File (Ctx, "test.adb");
   Unit2 : constant Analysis_Unit := Get_From_File (Ctx, "test2.adb");
begin
   Put_Line ("First and last tokens for test.adb:");
   Put_Line ("  * " & Image (First_Token (Unit)));
   Put_Line ("  * " & Image (Last_Token (Unit)));
   New_Line;

   Put_Line ("Whole source buffer for test.adb:");
   Put_Line (Image (Text (First_Token (Unit), Last_Token (Unit))));
   New_Line;

   declare
      Last  : constant Token_Reference := Next (First_Token (Unit));
      First : constant Token_Reference := Previous (Last_Token (Unit));
   begin
      Put_Line ("Empty range for the following bounds:");
      Put_Line ("  * " & Image (First));
      Put_Line ("  * " & Image (Last));
      Put_Line (Image (Text (First, Last), With_Quotes => True));
      New_Line;
   end;

   declare
      function Filter (N : Ada_Node) return Boolean is
        (N.Kind in Ada_Basic_Decl);

      procedure Proc (N : Ada_Node) is
      begin
         Put_Line ("  * " & N.Image);
         Put_Line ("    " & Image (N.Text));
         New_Line;
      end Proc;

      It : Ada_Node_Iterators.Iterator'Class :=
         Find (Root (Unit), Filter'Access);
   begin
      Put_Line ("Source excerpts for all Basic_Decl in test.adb:");
      Ada_Node_Iterators.Iterate (It, Proc'Access);
   end;

   Put_Line
     ("Trying to get a source slice for two nodes in different units...");
   begin
      Put_Line
        ("  -> " & Image (Text (First_Token (Unit), Last_Token (Unit2))));
      Put_Line ("... got no error: unacceptable!");
   exception
      when Constraint_Error =>
         Put_Line ("... got the expected Constraint_Error!");
   end;
   New_Line;

   Put_Line ("Done.");
end Main;
