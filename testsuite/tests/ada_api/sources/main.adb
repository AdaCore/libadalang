--  Unit tests for Libadalang.Sources

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.GMP.Integers;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Text;   use Langkit_Support.Text;

with Libadalang.Sources; use Libadalang.Sources;

procedure Main is
   procedure Test_Decode_Brackets (Input : Text_Type);
   procedure Test_Decode_Character_Literal (Input : Text_Type);
   procedure Test_Decode_String_Literal (Input : Text_Type);
   procedure Test_Decode_Integer_Literal (Input : Text_Type);
   procedure Put_Exception (Exc : Exception_Occurrence);

   --------------------------
   -- Test_Decode_Brackets --
   --------------------------

   procedure Test_Decode_Brackets (Input : Text_Type) is
      Error  : Boolean;
      Result : Wide_Wide_Character;
   begin
      Put ("Decode_Brackets (" & Image (Input, With_Quotes => True) & ") = ");
      Decode_Brackets (Input, Error, Result);
      Put_Line (if Error
                then "<error>"
                else Image ((1 => Result), With_Quotes => True));
   end Test_Decode_Brackets;

   -----------------------------------
   -- Test_Decode_Character_Literal --
   -----------------------------------

   procedure Test_Decode_Character_Literal (Input : Text_Type) is
   begin
      Put ("Decode_Character_Literal ("
           & Image (Input, With_Quotes => True) & ") = ");
      begin
         Put_Line (Image ((1 => Decode_Character_Literal (Input)),
                          With_Quotes => True));
      exception
         when Exc : Property_Error =>
            Put_Exception (Exc);
      end;
   end Test_Decode_Character_Literal;

   --------------------------------
   -- Test_Decode_String_Literal --
   --------------------------------

   procedure Test_Decode_String_Literal (Input : Text_Type) is
   begin
      Put ("Decode_String_Literal ("
           & Image (Input, With_Quotes => True) & ") = ");
      begin
         Put_Line (Image (Decode_String_Literal (Input), With_Quotes => True));
      exception
         when Exc : Property_Error =>
            Put_Exception (Exc);
      end;
   end Test_Decode_String_Literal;

   ---------------------------------
   -- Test_Decode_Integer_Literal --
   ---------------------------------

   procedure Test_Decode_Integer_Literal (Input : Text_Type) is
   begin
      Put ("Decode_Integer_Literal ("
           & Image (Input, With_Quotes => True) & ") = ");
      declare
         Result : GNATCOLL.GMP.Integers.Big_Integer;
      begin
         Decode_Integer_Literal (Input, Result);
         Put_Line (Result.Image);
      exception
         when Exc : Property_Error =>
            Put_Exception (Exc);
      end;
   end Test_Decode_Integer_Literal;

   -------------------
   -- Put_Exception --
   -------------------

   procedure Put_Exception (Exc : Exception_Occurrence) is
   begin
      Put_Line ("<Property_Error: " & Exception_Message (Exc) & ">");
   end Put_Exception;

begin
   Test_Decode_Brackets ("");
   Test_Decode_Brackets ("a");
   Test_Decode_Brackets ("[");
   Test_Decode_Brackets ("[]");
   Test_Decode_Brackets ("[""");
   Test_Decode_Brackets ("[""]");
   Test_Decode_Brackets ("[""""]");
   Test_Decode_Brackets ("[""0""]");
   Test_Decode_Brackets ("[""00""]");
   Test_Decode_Brackets ("[""00""]a");
   Test_Decode_Brackets ("[""0g""]");
   Test_Decode_Brackets ("[""12""]");
   Test_Decode_Brackets ("[""123""]");
   Test_Decode_Brackets ("[""1234""]");
   Test_Decode_Brackets ("[""12345""]");
   Test_Decode_Brackets ("[""123456""]");
   Test_Decode_Brackets ("[""1234567""]");
   Test_Decode_Brackets ("[""12345678""]");
   Test_Decode_Brackets ("[""123456789""]");
   Test_Decode_Brackets ("[""0000abcd""]");
   Test_Decode_Brackets ("[""0000ABCD""]");
   Test_Decode_Brackets ("[""ffffffff""]");
   New_Line;

   Test_Decode_Character_Literal ("");
   Test_Decode_Character_Literal ("a");
   Test_Decode_Character_Literal ("'");
   Test_Decode_Character_Literal ("'a");
   Test_Decode_Character_Literal ("'ab");
   Test_Decode_Character_Literal ("'a'");
   Test_Decode_Character_Literal ("'ab'");
   Test_Decode_Character_Literal ("'ab'a");
   New_Line;

   Test_Decode_String_Literal ("");
   Test_Decode_String_Literal ("a");
   Test_Decode_String_Literal ("""");
   Test_Decode_String_Literal ("""a");
   Test_Decode_String_Literal ("""""");
   Test_Decode_String_Literal ("""""a""");
   Test_Decode_String_Literal ("""""""");
   Test_Decode_String_Literal ("""a""");
   Test_Decode_String_Literal ("%a""");
   Test_Decode_String_Literal ("%a%");
   Test_Decode_String_Literal ("""""[""");
   Test_Decode_String_Literal ("""[""""");
   Test_Decode_String_Literal ("""[""]""");
   Test_Decode_String_Literal ("""[""""]""");
   Test_Decode_String_Literal ("""[""0""]""");
   Test_Decode_String_Literal ("""[""00""]""");
   Test_Decode_String_Literal ("""[""0g""]""");
   Test_Decode_String_Literal ("""[""00""");
   Test_Decode_String_Literal ("""[""00""""");
   New_Line;

   Test_Decode_Integer_Literal ("");
   Test_Decode_Integer_Literal ("a");
   Test_Decode_Integer_Literal ("0");
   Test_Decode_Integer_Literal ("16#1");
   Test_Decode_Integer_Literal ("16#1#");
   Test_Decode_Integer_Literal ("16#1##");
   Test_Decode_Integer_Literal ("16:1:");
   Test_Decode_Integer_Literal ("16#1:");
   Test_Decode_Integer_Literal ("#1#");
   Test_Decode_Integer_Literal ("-2#1#");
   Test_Decode_Integer_Literal ("17#1#");
   Test_Decode_Integer_Literal ("1000000000000000000000000000000000#1#");
   Test_Decode_Integer_Literal ("2#2#");
   Test_Decode_Integer_Literal ("3#2#");
   Test_Decode_Integer_Literal ("3#3#");
   Test_Decode_Integer_Literal ("16#f#0");
   Test_Decode_Integer_Literal ("16#f#a");
   Test_Decode_Integer_Literal ("16#f#e");
   Test_Decode_Integer_Literal ("16#f#ea");
   Test_Decode_Integer_Literal ("16#f#e+");
   Test_Decode_Integer_Literal ("16#f#e+1");
   Test_Decode_Integer_Literal ("16#f#e-1");
   Test_Decode_Integer_Literal ("1e+1");
   Test_Decode_Integer_Literal ("1ee1");
   New_Line;

   Put_Line ("Done.");
end Main;
