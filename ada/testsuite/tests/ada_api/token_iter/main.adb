with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

procedure Main is

   procedure Process (Filename : String; With_Trivia : Boolean);

   -------------
   -- Process --
   -------------

   procedure Process (Filename : String; With_Trivia : Boolean) is
      Ctx  : constant Analysis_Context :=
         Create_Context (With_Trivia => With_Trivia);
      Unit : constant Analysis_Unit := Get_From_File (Ctx, Filename);

      Token      : Token_Reference := First_Token (Unit);
      Prev_Token : Token_Reference := No_Token;
   begin
      Put ("Tokens for " & Filename & " ");
      Put (if With_Trivia then "(with trivia)" else "(no trivia)");
      Put_Line (":");

      while Token /= No_Token loop
         declare
            PT : constant Token_Reference := Previous (Token);
         begin
            if Prev_Token /= PT then
               raise Program_Error;
            end if;
         end;

         declare
            TD : constant Token_Data_Type := Data (Token);
         begin
            Put_Line
              ("  [" & (if Is_Trivia (TD) then "trivia" else "token ")
               & Token_Index'Image (Index (TD)) & "] "
               & Token_Kind_Name (Kind (TD))
               & " " & Image (Text (Token), With_Quotes => True));
         end;
         Prev_Token := Token;
         Token := Next (Token);
      end loop;
      New_Line;
   end Process;

   type String_Array is array (Positive range <>) of Unbounded_String;
   function "+" (S : String) return Unbounded_String
      renames To_Unbounded_String;

   No_Trivia_Tests : constant String_Array :=
     (+"no_trivia.adb",
      +"empty.adb");
   Trivia_Tests    : String_Array :=
      No_Trivia_Tests
      & (+"one_leading_comment.adb",
         +"two_leading_comments.adb",
         +"one_middle_comment.adb",
         +"two_middle_comments.adb",
         +"one_trailing_comment.adb",
         +"two_trailing_comments.adb",
         +"only_one_comment.adb",
         +"only_two_comments.adb");
begin
   for Filename of No_Trivia_Tests loop
      Process (To_String (Filename), False);
   end loop;

   for Filename of Trivia_Tests loop
      Process (To_String (Filename), True);
   end loop;

   Put_Line ("Done.");
end Main;
