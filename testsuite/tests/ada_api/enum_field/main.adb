with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is

   Ctx    : constant Analysis_Context := Create_Context;
   Unit   : constant Analysis_Unit := Get_From_File (Ctx, "test.adb");
   Params : constant Ada_Node_Array :=
      Find (Unit.Root, Kind_Is (Ada_Param_Spec)).Consume;
begin
   for P of Params loop
      declare
         PP   : constant Param_Spec := P.As_Param_Spec;
         Name : constant Text_Type := PP.F_Ids.Child (1).Text;
      begin
         Put_Line (Image (Name) & " is a: " & Ada_Mode'Image (PP.F_Mode));
      end;
   end loop;
   Put_Line ("Done.");
end Main;
