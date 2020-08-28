with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

procedure Main is

   Filename : String := "test.adb";

   Ctx   : constant Analysis_Context :=
      Create_Context (With_Trivia => False);
   Unit  : constant Analysis_Unit := Get_From_File (Ctx, Filename);
   Token : Token_Reference := First_Token (Unit);

   subtype Version_Image is String (1 .. 2);
   Version_Images : constant array (Language_Version)
                             of Version_Image := ("83", "95", "05", "12");

begin
   Put_Line ("Keyword tokens for " & Filename & ":");

   while Token /= No_Token loop
      declare
         TD     : constant Token_Data_Type := Data (Token);
         Result : array (Language_Version) of Boolean;
      begin
         for Version in Language_Version loop
            Result (Version) := Is_Keyword (Token, Version);
         end loop;

         if (for some Version in Language_Version => Result (Version)) then
            for Version in Language_Version loop
               if Result (Version) then
                  Put (Version_Images (Version) & " ");
               else
                  Put ("   ");
               end if;
            end loop;
            Put_Line (Image (Text (Token), With_Quotes => True)
                      & " [" & Image (Start_Sloc (Sloc_Range (TD))) & "]");
         end if;
      end;
      Token := Next (Token);
   end loop;

   New_Line;
   Put_Line ("Done.");
end Main;
