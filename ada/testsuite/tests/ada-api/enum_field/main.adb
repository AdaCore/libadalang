with Ada.Text_IO;          use Ada.Text_IO;

with Langkit_Support.Text;  use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Lexer;    use Libadalang.Lexer;

procedure Main is

   Ctx    : Analysis_Context := Create;
   Unit   : constant Analysis_Unit := Get_From_File (Ctx, "test.adb");
   P      : constant Ada_Node_Predicate := new Ada_Node_Kind_Filter'
     (Kind => Ada_Param_Spec);
   Params : constant Ada_Node_Vectors.Elements_Array :=
      Root (Unit).Find (P).Consume;
begin
   for P of Params loop
      declare
         PP        : constant Param_Spec := Param_Spec (P);
         Name      : constant Token_Type :=
            Identifier (PP.F_Ids.Child (1)).F_Tok;
         Name_Text : constant Text_Type := Data (Name).Text.all;
      begin
         Put_Line
           (Image (Name_Text) & " is a: " & Ada_Mode'Image (PP.F_Mode));
      end;
   end loop;
   Destroy (Ctx);
   Put_Line ("Done.");
end Main;
