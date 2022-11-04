--  Test that a node set instanciated with the proper Ada_Node.Equals function
--  for testing equivalence between nodes works correcly.

with Ada.Containers.Hashed_Sets;
with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is

   procedure Include (Val : Ada_Node);

   Ctx : constant Analysis_Context := Create_Context;

   Unit : constant Analysis_Unit := Ctx.Get_From_File ("pkg.ads");

   Type_Decls : constant Ada_Node_Array :=
     Find
       (Unit.Root,
        Kind_In (Ada_Type_Decl'First, Ada_Type_Decl'Last)).Consume;

   TD_1 : constant Type_Decl := Type_Decls (1).As_Type_Decl;
   TD_2 : constant Type_Decl := Type_Decls (2).As_Type_Decl;

   Prim_1 : constant Ada_Node := TD_1.P_Get_Primitives (1).As_Ada_Node;
   Prim_2 : constant Ada_Node := TD_2.P_Get_Primitives (1).As_Ada_Node;
   Subp   : constant Expr_Function := 
      Find (Unit.Root, Kind_Is (Ada_Expr_Function))
      .Consume (2).As_Expr_Function;
   Prim_3 : constant Basic_Decl :=
     Subp.F_Expr.Child (1).As_Name.P_Referenced_Decl;

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Ada_Node,
      Hash                => Hash,
      Equivalent_Elements => Equals,
      "="                 => Equals);

   Set : Node_Sets.Set;

   use Ada.Containers;


   procedure Include (Val : Ada_Node) is
   begin
      Put_Line ("Including " & Val.Image & " in set");
      Set.Include (Val);
   end Include;

begin
   Include (Prim_1);
   Include (Prim_2);
   Put_Line ("Set length = " & Set.Length'Image & " (should be 2)");
   pragma Assert (Set.Length = 2);
   Include (Prim_3.As_Ada_Node);
   Put_Line ("Set length = " & Set.Length'Image & " (should be 2)");
   Put_Line ("Done.");
end Main;
