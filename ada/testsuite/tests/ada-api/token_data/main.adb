with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.AST;       use Libadalang.AST;
with Libadalang.AST.Types; use Libadalang.AST.Types;
with Libadalang.Lexer;     use Libadalang.Lexer;

procedure Main is
   Ctx    : Analysis_Context := Create;
   Unit   : Analysis_Unit := Get_From_File (Ctx, "foo.adb");
   N      : constant Ada_Node := Root (Unit).Find_First
     (new Ada_Node_Kind_Filter'(Kind => Ada_Identifier));
begin
   declare
      Id       : constant Single_Tok_Node := Single_Tok_Node (N);
      Tok      : constant Token_Type := F_Tok (Id);
      Tok_Data : constant Token_Data_Type := Data (Tok);
   begin
      Put_Line ("Token data for the ""foo"" identifier:");
      Put_Line ("Kind: " & Token_Kind_Name (Tok_Data.Kind));
      Put_Line ("Text: " & Image (Tok_Data.Text.all));
      Put_Line ("Sloc range: " & Image (Tok_Data.Sloc_Range));
   end;

   Destroy (Ctx);
   Put_Line ("Done.");
end Main;
