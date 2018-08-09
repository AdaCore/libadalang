with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Text;  use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;
with Libadalang.Lexer;

procedure Main is

   subtype Token_Index is Libadalang.Lexer.Token_Data_Handlers.Token_Index;

   Filename : String := "test.adb";

   Ctx      : constant Analysis_Context := Create (With_Trivia => False);
   Unit     : constant Analysis_Unit := Get_From_File (Ctx, Filename);

   Token    : Token_Reference := First_Token (Unit);

begin
   Put_Line ("Keyword tokens for " & Filename & ":");

   while Token /= No_Token loop
      declare
         TD : constant Token_Data_Type := Data (Token);
      begin
         for Lang in Language_Version loop
            if not Is_Trivia (TD) then
               if Is_Keyword (Ctx, Token, Lang) then
                  Put_Line
                    ("Version = " & Lang'Image & ", "
                     & Image (Text (Token), With_Quotes => True)
                     & " Is_Reserved");
               end if;
            end if;
         end loop;
      end;
      Token := Next (Token);
   end loop;

   New_Line;
   Put_Line ("Done.");
end Main;
