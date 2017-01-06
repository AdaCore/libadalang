with Ada.Text_IO;          use Ada.Text_IO;

with Langkit_Support.Text;  use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Lexer;    use Libadalang.Lexer;

procedure Main is

   Ctx       : Analysis_Context := Create;
   Unit      : constant Analysis_Unit := Get_From_File (Ctx, "pkg.ads");
   P         : constant Ada_Node_Predicate := new Ada_Node_Kind_Filter'
     (Kind => Ada_Type_Decl);
   Type_Defs : constant Ada_Node_Vectors.Elements_Array :=
      Root (Unit).Find (P).Consume;
begin
   for T of Type_Defs loop
      declare
         TD  : constant Type_Decl := Type_Decl (T);
         RTD : constant Record_Type_Def := Record_Type_Def (TD.F_Type_Def);

         Name      : constant Token_Type := TD.F_Type_ID.F_Tok;
         Name_Text : constant Text_Type := Text (Name);
      begin
         Put_Line
           (Image (Name_Text) & " is abstract: "
            & Boolean'Image (RTD.F_Has_Abstract));
      end;
   end loop;
   Destroy (Ctx);
   Put_Line ("Done.");
end Main;
