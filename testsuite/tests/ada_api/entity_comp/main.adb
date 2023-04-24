with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is

   Ctx : constant Analysis_Context := Create_Context;

   Unit       : constant Analysis_Unit := Ctx.Get_From_File ("pkg.ads");
   Type_Decls : constant Ada_Node_Array :=
      Find
        (Unit.Root,
         Kind_In (Ada_Base_Type_Decl'First, Ada_Type_Decl'Last)).Consume;

   D1 : constant Type_Decl := Type_Decls (1).As_Type_Decl;
   D2 : constant Type_Decl := Type_Decls (2).As_Type_Decl;

   Prim_1 : constant Basic_Decl := D1.P_Get_Primitives (1);
   Prim_2 : constant Basic_Decl := D2.P_Get_Primitives (1);

   E : constant Ada_Node :=
     Find
       (Unit.Root,
        Kind_In (Ada_Base_Subp_Body'First, Ada_Base_Subp_Body'Last))
     .Consume (2);

   Prim_3 : constant Basic_Decl :=
     E.As_Expr_Function.F_Expr.Child (1).As_Name.P_Referenced_Decl;
begin
   Put_Line
     ("Prim_T.Primitive = Der_T.Primitive: "
      & Boolean'Image (Prim_1 = Prim_2));

   Put_Line (E.As_Expr_Function.F_Expr.Child (1).As_Name.Image);
   Put_Line (Prim_3.Image);

   Put_Line
     ("Der_T.Primitive = Der_T.Primitive (different internal metadata): "
      & Boolean'Image (Prim_2 = Prim_3));

   Put_Line ("Done.");
end Main;
