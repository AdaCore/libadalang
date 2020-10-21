with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context;
   Unit : constant Analysis_Unit := Get_From_File (Ctx, "foo.adb");
   N    : constant Ada_Node := Find_First
     (Unit.Root, Kind_Is (Ada_Identifier));
begin
   declare
      Id       : constant Single_Tok_Node := N.As_Single_Tok_Node;
      Tok      : constant Token_Reference := Id.Token_Start;
      Tok_Data : constant Token_Data_Type := Data (Tok);
   begin
      Put_Line ("Token data for the ""foo"" identifier:");
      Put_Line ("Kind: " & Token_Kind_Name (Kind (Tok_Data)));
      Put_Line ("Text: " & Image (Text (Tok)));
      Put_Line ("Sloc range: " & Image (Sloc_Range (Tok_Data)));
   end;

   Put_Line ("Done.");
end Main;
