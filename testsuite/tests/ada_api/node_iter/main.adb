with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx    : constant Analysis_Context := Create_Context;
   Unit   : constant Analysis_Unit := Get_From_File (Ctx, "foo.adb");
   Params : constant Param_Spec_List :=
      Find_First (Unit.Root, Kind_Is (Ada_Param_Spec_List)).As_Param_Spec_List;

begin
   for P of Params loop
      Put_Line (Image (P.F_Ids.Text));
   end loop;

   Put_Line ("Done.");
end Main;
