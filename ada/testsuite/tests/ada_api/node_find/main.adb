with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Text_IO;                use Ada.Text_IO;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context;
   Unit : constant Analysis_Unit := Get_From_File (Ctx, "foo.adb");

   procedure Put_Node (N : Ada_Node);
   --  Put the image of N on the standard output

   procedure Run_Find
     (Filename, Label : String;
      Predicate       : Ada_Node_Predicate);
   --  Load the Filename unit and then show all nodes that Predicate matches in
   --  this unit.

   --------------
   -- Put_Node --
   --------------

   procedure Put_Node (N : Ada_Node) is
   begin
      Put_Line ("  " & N.Image);
   end Put_Node;

   --------------
   -- Run_Find --
   --------------

   procedure Run_Find
     (Filename, Label : String;
      Predicate       : Ada_Node_Predicate) is
   begin
      Put_Line ("[" & Filename & "] " & Label & ":");
      for Node of Find (Ctx.Get_From_File (Filename).Root, Predicate).Consume
      loop
         Put_Node (Node);
      end loop;
      New_Line;
   end Run_Find;

begin
   declare
      It : Ada_Node_Iterators.Iterator'Class := Traverse (Unit.Root);
   begin
      Put_Line ("[foo.adb] All nodes from root:");
      Ada_Node_Iterators.Iterate (It, Put_Node'Access);
      New_Line;
   end;

   Put_Line
     ("[foo.adb] All nodes from null:");
   for Node of Traverse (No_Ada_Node).Consume loop
      Put_Node (Node);
   end loop;
   New_Line;

   Run_Find ("foo.adb", "All identifiers", Kind_Is (Ada_Identifier));
   Run_Find ("pkg-foo.ads", "All declarations of Foo",
             Decl_Defines ("Foo"));
   Run_Find ("pkg-foo.ads", "All declarations of ""+""",
             Decl_Defines ("""+"""));

   Run_Find ("pkg-foo.ads",
             "All declarations of Foo that are types or components",
             Decl_Defines ("Foo") and (Kind_Is (Ada_Type_Decl)
                                       or Kind_Is (Ada_Component_Decl)));

   declare
      Foo_Type : constant Type_Decl := Find_First
        (Ctx.Get_From_File ("pkg.ads").Root,
         Kind_Is (Ada_Type_Decl)).As_Type_Decl;
   begin
      Run_Find ("pkg.ads", "All references to the Foo type",
                Xref_Is (Foo_Type.F_Name));
   end;

   Put_Line ("Done.");
end Main;
