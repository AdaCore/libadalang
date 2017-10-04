with Ada.Text_IO;          use Ada.Text_IO;

with Langkit_Support.Text;  use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is

   Ctx    : Analysis_Context := Create;
   Unit   : constant Analysis_Unit := Get_From_File (Ctx, "test.adb");
   P      : constant Ada_Node_Predicate := new Ada_Node_Kind_Filter'
     (Kind => Ada_Param_Spec);
   Params : constant Ada_Node_Array :=
      Ada_Node_Array (Find (Root (Unit), P).Consume);
begin
   for P of Params loop
      declare
         PP        : constant Param_Spec := P.As_Param_Spec;
         Name      : constant Token_Type :=
            PP.F_Ids.Child (1).As_Identifier.F_Tok;
         Name_Text : constant Text_Type := Text (Name);
      begin
         Put_Line
           (Image (Name_Text) & " is a: " & Ada_Mode'Image (PP.F_Mode));
      end;
   end loop;
   Destroy (Ctx);
   Put_Line ("Done.");
end Main;
