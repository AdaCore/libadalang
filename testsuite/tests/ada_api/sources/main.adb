--  Unit tests for Libadalang.Sources

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.GMP.Integers;
with GNATCOLL.GMP.Rational_Numbers;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Text;   use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;
with Libadalang.Sources;   use Libadalang.Sources;

procedure Main is
   procedure Test_Decode_Brackets (Input : Text_Type);
   procedure Test_Decode_Character_Literal (Input : Text_Type);
   procedure Test_Decode_String_Literal (Input : Text_Type);
   procedure Test_Decode_Integer_Literal (Input : Text_Type);
   procedure Test_Decode_Real_Literal (Input : Text_Type);
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
      use type GNATCOLL.GMP.Integers.Big_Integer;
      Result : Big_Integer;
   begin
      Put ("Decode_Integer_Literal ("
           & Image (Input, With_Quotes => True) & ") = ");
      begin
         Decode_Integer_Literal (Input, Result);
         Put_Line (Result.Image);
      exception
         when Exc : Property_Error =>
            Put_Exception (Exc);
            return;
      end;

      --  Check that Encode_Integer_Literal returns code that denotes the same
      --  value.

      declare
         Literal : constant Text_Type := Encode_Integer_Literal (Result);
         Decoded : Big_Integer;
      begin
         Decode_Integer_Literal (Literal, Decoded);
         if Decoded /= Result then
            raise Program_Error;
         end if;
      end;
   end Test_Decode_Integer_Literal;

   ------------------------------
   -- Test_Decode_Real_Literal --
   ------------------------------

   procedure Test_Decode_Real_Literal (Input : Text_Type) is
      use type GNATCOLL.GMP.Rational_Numbers.Rational;
      Result : Rational;
   begin
      Put ("Decode_Real_Literal ("
           & Image (Input, With_Quotes => True) & ") = ");
      begin
         Decode_Real_Literal (Input, Result);
         Put_Line (Result.Image);
      exception
         when Exc : Property_Error =>
            Put_Exception (Exc);
            return;
      end;

      --  Check that Encode_Real returns code that denotes the same value

      declare
         Buffer : constant String :=
           "package P is" & ASCII.LF
           & "   N : constant := " & Image (Encode_Real (Result)) & ";"
           & ASCII.LF
           & "end P;" & ASCII.LF;

         U : constant Analysis_Unit :=
           Create_Context.Get_From_Buffer ("foo.ads", Buffer => Buffer);

         Error : Unbounded_String;

         Num, Den : Big_Integer;
         Decoded  : Rational;
      begin
         if U.Has_Diagnostics then
            for D of U.Diagnostics loop
               Put_Line (U.Format_GNU_Diagnostic (D));
            end loop;
            Append (Error, "Encode_Real yielded unparsable code");
         else
            declare
               N     : constant Number_Decl :=
                 U.Root
                 .As_Compilation_Unit.F_Body
                 .As_Library_Item.F_Item
                 .As_Package_Decl.F_Public_Part
                 .F_Decls.Child (1).As_Number_Decl;
               Value : constant Eval_Result := Expr_Eval (N.F_Expr);
            begin
               if Value.Kind /= Real then
                  Append (Error,
                          "Encode_Real yielded a non-real: " & Image (Value));
               else
                  Decoded.Set (Value.Real_Result);
                  if Decoded /= Result then
                     Append (Error, "Encode_Real yielded " & Decoded.Image);
                  end if;
               end if;
            end;
         end if;

         if Error /= Null_Unbounded_String then
            Put_Line ("ERROR: " & To_String (Error));
            Put_Line (Buffer);
            raise Program_Error;
         end if;
      end;
   end Test_Decode_Real_Literal;

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
   Test_Decode_Integer_Literal ("1.0");
   Test_Decode_Integer_Literal ("1.");
   New_Line;

   Test_Decode_Real_Literal ("1");
   Test_Decode_Real_Literal ("1.0.0");
   Test_Decode_Real_Literal ("1..0");
   Test_Decode_Real_Literal ("1.");
   Test_Decode_Real_Literal ("1.0");
   Test_Decode_Real_Literal ("1.00");
   Test_Decode_Real_Literal ("0.0");
   Test_Decode_Real_Literal ("0.01");
   Test_Decode_Real_Literal ("8#1.0#");
   Test_Decode_Real_Literal ("1.0e1");
   Test_Decode_Real_Literal ("1.0e0");
   Test_Decode_Real_Literal ("1.0e-1");
   Test_Decode_Real_Literal ("1.0e-0");
   Test_Decode_Real_Literal ("1.0e10");
   Test_Decode_Real_Literal ("1.0e-10");
   Test_Decode_Real_Literal ("1_0.0_0e10");
   Test_Decode_Real_Literal ("1_0.0_0e-10");
   Test_Decode_Real_Literal ("8#1.0#E2");
   Test_Decode_Real_Literal ("8#1.0#E-2");
   Test_Decode_Real_Literal ("0.33");
   Test_Decode_Real_Literal ("0.333");
   Test_Decode_Real_Literal ("12345.67890");
   Test_Decode_Real_Literal ("09876.54321");
   Test_Decode_Real_Literal ("09876.54321E+012");
   Test_Decode_Real_Literal ("09876.54321E-012");
   New_Line;

   Put_Line ("Done.");
end Main;
