with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context;
   Unit : Analysis_Unit := Get_From_File (Ctx, "foo.adb");
   CU   : constant Compilation_Unit := Root (Unit).As_Compilation_Unit;

   function Find_Binops (N : Ada_Node) return Boolean
   is (N.Kind = Ada_Bin_Op);

   BO_Array : constant Ada_Node_Array := Find (CU, Find_Binops'Access).Consume;
   BO       : constant Bin_Op := BO_Array (1).As_Bin_Op;
begin
   Put_Line ("Tokens for node " & BO.Image & ":");
   for Tok of BO.Token_Range loop
      Put_Line (Image (Tok));
   end loop;
end Main;
