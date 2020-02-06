------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2020, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Wide_Wide_Characters.Handling;

with Interfaces; use Interfaces;

package body Libadalang.Sources is

   ---------------------
   -- Decode_Brackets --
   ---------------------

   procedure Decode_Brackets
     (Pattern : Text_Type;
      Error   : out Boolean;
      Result  : out Wide_Wide_Character) is
   begin
      if Pattern'Length < 6 or else Pattern'Length > 12
         or else Pattern (Pattern'First .. Pattern'First + 1) /= "["""
         or else Pattern (Pattern'Last - 1 .. Pattern'Last) /= """]"
      then
         Error := True;
         return;
      end if;

      declare
         Digits_String : Text_Type renames Pattern
           (Pattern'First + 2 ..  Pattern'Last - 2);
         subtype Valid_Codepoint is Unsigned_32
            range Wide_Wide_Character'Pos (Wide_Wide_Character'First)
               .. Wide_Wide_Character'Pos (Wide_Wide_Character'Last);
         Codepoint : Unsigned_32 := 0;
      begin
         if Digits_String'Length not in 2 | 4 | 6 | 8 then
            Error := True;
            return;
         end if;

         for C of Digits_String loop
            declare
               Charcode : constant Unsigned_32 := Wide_Wide_Character'Pos (C);
               Digit    : Unsigned_32;
            begin
               if C in '0' .. '9' then
                  Digit := Charcode - Wide_Wide_Character'Pos ('0');
               elsif C in 'A' .. 'F' then
                  Digit := Charcode - Wide_Wide_Character'Pos ('A') + 10;
               elsif C in 'a' .. 'f' then
                  Digit := Charcode - Wide_Wide_Character'Pos ('a') + 10;
               else
                  Error := True;
                  return;
               end if;
               Codepoint := 16 * Codepoint + Digit;
            end;
         end loop;

         if Codepoint not in Valid_Codepoint then
            Error := True;
            return;
         end if;

         Result := Wide_Wide_Character'Val (Codepoint);
         Error := False;
      end;
   end Decode_Brackets;

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize (Name : Text_Type) return Symbolization_Result is
      Result      : Text_Type (Name'Range);
      Result_Last : Integer := Name'First - 1;

      I : Positive := Name'First;
   begin
      --  Decode bracket encodings

      while I <= Name'Last loop

         --  First, try to decode a brackets encoded char, if any

         Result_Last := Result_Last + 1;
         declare
            C     : constant Wide_Wide_Character := Name (I);
            J     : Positive := I + 1;
            Error : Boolean;
         begin
            if C = '['
               and then J in Name'Range
               and then Name (J) = '"'
            then
               --  If we have the [" sequence, start decoding the brackets
               --  construct.

               while J < Name'Last and then Name (J) /= ']' loop
                  J := J + 1;
               end loop;
               Decode_Brackets (Name (I .. J), Error, Result (Result_Last));
               if Error then
                  return Create_Error ("invalid brackets encoding");
               end if;
               I := J;

            else
               --  Otherwise, just copy the chararcters

               Result (Result_Last) := C;
            end if;
         end;

         --  Now, perform case folding. Optimization: don't do costly wide wide
         --  To_Lower for ASCII: this is the most common case by far and we can
         --  handle it very quickly here with several range checks.

         declare
            subtype WWC is Wide_Wide_Character;

            First_ASCII : constant WWC :=
               WWC'Val (Character'Pos (ASCII.NUL));
            Last_ASCII  : constant WWC :=
               WWC'Val (Character'Pos (ASCII.DEL));
            subtype WWC_ASCII is WWC range First_ASCII .. Last_ASCII;

            C : WWC renames Result (Result_Last);
         begin
            if C in 'A' .. 'Z' then
               C := WWC'Val (WWC'Pos (C) - WWC'Pos ('A') + WWC'Pos ('a'));
            elsif C in WWC_ASCII'Range then
               null;
            else
               C := Ada.Wide_Wide_Characters.Handling.To_Lower (C);
            end if;
         end;

         I := I + 1;
      end loop;

      return Create_Symbol (Result (Result'First .. Result_Last));
   end Canonicalize;

   ------------------------------
   -- Decode_Character_Literal --
   ------------------------------

   function Decode_Character_Literal (Text : Text_Type) return Character_Type
   is
   begin
      if Text'Length /= 3
         or else Text (Text'First) /= '''
         or else Text (Text'Last) /= '''
      then
         raise Libadalang.Common.Property_Error
            with "Invalid character literal";
      end if;

      return Text (Text'First + 1);
   end Decode_Character_Literal;

   ---------------------------
   -- Decode_String_Literal --
   ---------------------------

   function Decode_String_Literal (Text : Text_Type) return Text_Type is

      Result : Text_Type (Text'Range);
      Last   : Natural := Result'First - 1;

      procedure Error;
      procedure Put (C : Character_Type);

      -----------
      -- Error --
      -----------

      procedure Error is
      begin
         raise Libadalang.Common.Property_Error
            with "Invalid string literal";
      end Error;

      ---------
      -- Put --
      ---------

      procedure Put (C : Character_Type) is
      begin
         Last := Last + 1;
         Result (Last) := C;
      end Put;

      Delimiter         : Character_Type;
      Last_Is_Delimiter : Boolean := False;
      I                 : Natural;
   begin
      --  TODO: handle brackets encoding

      --  Ensure we have valid delimiters at the start and the end of the input
      --  text, and ensure they are the same delimiters.

      if Text'Length < 2
         or else Text (Text'First) not in '"' | '%'
         or else Text (Text'First) /= Text (Text'Last)
      then
         Error;
      end if;
      Delimiter := Text (Text'First);

      I := Text'First + 1;
      while I < Text'Last loop
         declare
            C : constant Character_Type := Text (I);
         begin
            if C = Delimiter then
               if Last_Is_Delimiter then
                  Put (C);
                  Last_Is_Delimiter := False;
               else
                  Last_Is_Delimiter := True;
               end if;

            elsif C = '[' then
               if Last_Is_Delimiter then
                  Error;
               end if;

               --  If this is not a brackets encoding, just forward characters
               --  to Result, otherwise check that it is a valid one.
               if
                  I + 1 = Text'Last
                  --  C is the last character

                  or else Text (I + 1) /= '"'
                  --  C is not followed by '"' ("[a" is not a brackets
                  --  encoding).

                  or else (I + 2 < Text'Last and then Text (I + 2) = '"')
                  --  C is followed by two quotes ("[""" is not a brackets
                  --  encoding).
               then
                  Put (C);

               else
                  --  Thanks to the above condition, we know that at this point
                  --  Text (I) is followed by a quote: look for the closing
                  --  sequence ('"' followed by ']').
                  declare
                     Closing_Bracket : Natural := 0;
                  begin
                     for J in I + 2 .. Text'Last - 1 loop
                        if Text (J) = '"' then
                           Closing_Bracket := J + 1;
                           if Closing_Bracket > Text'Last - 1
                              or else Text (Closing_Bracket) /= ']'
                           then
                              Error;
                           end if;
                           exit;

                        elsif Text (J) not in '0' .. '9'
                                            | 'a' .. 'f'
                                            | 'A' .. 'F'
                        then
                           Error;
                        end if;
                     end loop;

                     --  Make sure we found the closing sequence, then decode
                     --  it and move on to what comes next...
                     if Closing_Bracket = 0 then
                        Error;
                     end if;

                     declare
                        Denoted_Char : Character_Type;
                        Has_Error    : Boolean;
                     begin
                        Decode_Brackets (Text (I .. Closing_Bracket),
                                         Has_Error, Denoted_Char);
                        if Has_Error then
                           Error;
                        end if;
                        Put (Denoted_Char);
                     end;
                     I := Closing_Bracket;
                  end;
               end if;

            else
               if Last_Is_Delimiter then
                  Error;
               else
                  Put (C);
               end if;
            end if;
         end;
         I := I + 1;
      end loop;

      if Last_Is_Delimiter then
         Error;
      end if;

      return Result (Result'First .. Last);
   end Decode_String_Literal;

   -------------------------------
   -- Numeric literals handling --
   -------------------------------

   type String_Slice is record
      First : Positive;
      Last  : Natural;
   end record;
   --  First class citizen type for string slices, with same semantics for
   --  bounds: they are inclusive and Last < First means an empty slice.

   subtype Numerical_Base is Natural range 2 .. 16;

   type Parsed_Numeric_Literal is record
      Base : Numerical_Base;
      --  Base for the numeric literal

      Numeral : String_Slice;
      --  Slice for the basic number, to be interpreted in base 10 if Base is
      --  empty, or in the corresponding base otherwise. This slice cannot be
      --  empty and can contain underscores.

      Exponent : Integer;
      --  Exponent to apply to Numeral, so that the designated number is::
      --
      --     Numeral * 10 ** Exponent.
   end record;
   --  Result of the analysis of a numeric literal string

   function Parse_Numeric_Literal
     (Text : Text_Type) return Parsed_Numeric_Literal;
   --  Parse Text as an Ada numeric literal. Raise a
   --  Libadalang.Common.Property_Error if it is invalid. Otherwise, return
   --  information about it.

   function Strip_Underscores (Text : Text_Type) return String;
   --  Turn Text, a wide wide string that contains only alphanumerics and
   --  underscores, into a simple string, with the underscores stripped.

   function Evaluate_Simple_Number (Text : Text_Type) return Integer is
     (Integer'Value (Strip_Underscores (Text)));

   procedure Error;
   --  Raise a Property_Error for an invalid numeric literal

   -----------------------
   -- Strip_Underscores --
   -----------------------

   function Strip_Underscores (Text : Text_Type) return String is

      Result : String (Text'Range);
      Next   : Natural := Result'First;

      procedure Put (I : Positive);
      --  Append Text (I) to Result, incrementing Next

      ---------
      -- Put --
      ---------

      procedure Put (I : Positive) is
      begin
         Result (Next) := Character'Val (Wide_Wide_Character'Pos (Text (I)));
         Next := Next + 1;
      end Put;

   begin
      for I in Text'Range loop
         case Text (I) is
            when '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' => Put (I);

            when '+' | '-'  =>
               if I /= Text'First then
                  Error;
               else
                  Put (I);
               end if;

            when '_'    => null;
            when others => Error;
         end case;
      end loop;
      return Result (Result'First .. Next - 1);
   end Strip_Underscores;

   -----------
   -- Error --
   -----------

   procedure Error is
   begin
      raise Libadalang.Common.Property_Error with "invalid numeric literal";
   end Error;

   ---------------------------
   -- Parse_Numeric_Literal --
   ---------------------------

   function Parse_Numeric_Literal
     (Text : Text_Type) return Parsed_Numeric_Literal
   is

      subtype Index is Integer range Text'First - 1 .. Text'Last;
      No_Index : constant Index := Index'First;

      Result : Parsed_Numeric_Literal;

      Base_First_Delimiter  : Index := No_Index;
      Base_Second_Delimiter : Index := No_Index;
   begin
      if Text = "" then
         Error;
      end if;

      --  First, look for the two base delimiters ('#' or ':' characters)
      for I in Text'Range loop
         if Text (I) in '#' | ':' then
            if Base_Second_Delimiter /= No_Index then
               Error;
            elsif Base_First_Delimiter /= No_Index then
               Base_Second_Delimiter := I;
            else
               Base_First_Delimiter := I;
            end if;
         end if;
      end loop;

      --  Either only two are present, either no one is
      if Base_First_Delimiter /= No_Index
         and then Base_Second_Delimiter = No_Index
      then
         Error;
      end if;

      if Base_First_Delimiter /= No_Index then
         --  Decode the base, when present
         declare
            Base_Image : Text_Type renames
               Text (Text'First .. Base_First_Delimiter - 1);
         begin
            if Base_Image'Length = 0 then
               Error;
            end if;

            declare
               Base : constant Integer := Evaluate_Simple_Number (Base_Image);
            begin
               if Base not in Numerical_Base then
                  Error;
               end if;
               Result.Base := Base;
            end;
         exception
            when Constraint_Error =>
               Error;
         end;

         Result.Numeral := (Base_First_Delimiter + 1,
                            Base_Second_Delimiter - 1);

         --  Decode the exponent, if present
         if Base_Second_Delimiter < Text'Last then
            if Text (Base_Second_Delimiter + 1) not in 'e' | 'E' then
               Error;
            end if;
            begin
               Result.Exponent := Evaluate_Simple_Number
                 (Text (Base_Second_Delimiter + 2 ..  Text'Last));
            exception
               when Constraint_Error =>
                  Error;
            end;
         else
            Result.Exponent := 0;
         end if;

      else
         --  When absent, fallback to its default and look for an exponent
         Result.Base := 10;

         --  Look for an exponent: whether we find it or not, we'll then know
         --  how the numeral spans.
         declare
            Exponent_Delimiter : Index := No_Index;
         begin
            for I in Text'Range loop
               if Text (I) in 'E' | 'e' then
                  if Exponent_Delimiter /= No_Index then
                     Error;
                  end if;
                  Exponent_Delimiter := I;
               end if;
            end loop;

            Result.Numeral.First := Text'First;
            if Exponent_Delimiter /= No_Index then
               Result.Numeral.Last := Exponent_Delimiter - 1;
               Result.Exponent := Evaluate_Simple_Number
                 (Text (Exponent_Delimiter + 1 .. Text'Last));
            else
               Result.Numeral.Last := Text'Last;
               Result.Exponent := 0;
            end if;
         end;
      end if;

      --  Make sure the numeral only uses digits allowed by the base
      declare

         function Rebase
           (Value, From, To : Wide_Wide_Character) return Wide_Wide_Character;

         ------------
         -- Rebase --
         ------------

         function Rebase
           (Value, From, To : Wide_Wide_Character) return Wide_Wide_Character
         is
            From_Pos : constant Natural := Wide_Wide_Character'Pos (From);
            To_Pos   : constant Natural := Wide_Wide_Character'Pos (To);
            Offset   : constant Integer := To_Pos - From_Pos;
         begin
            return Wide_Wide_Character'Val
              (Wide_Wide_Character'Pos (Value) + Offset);
         end Rebase;

         Digits_First : constant Wide_Wide_Character := '0';
         Digits_Last  : constant Wide_Wide_Character :=
           (if Result.Base <= 10
            then Wide_Wide_Character'Val (Wide_Wide_Character'Pos ('0')
                                          + Result.Base - 1)
            else '9');

         Lower_Ext_Digits_First : constant Wide_Wide_Character := 'a';
         Lower_Ext_Digits_Last  : constant Wide_Wide_Character :=
            Wide_Wide_Character'Val
              (Wide_Wide_Character'Pos (Lower_Ext_Digits_First)
               + Result.Base - 9);

         Upper_Ext_Digits_First : constant Wide_Wide_Character := 'A';
         Upper_Ext_Digits_Last  : constant Wide_Wide_Character :=
            Rebase (Lower_Ext_Digits_Last, 'a', 'A');

         C : Wide_Wide_Character;
      begin
         for I in Result.Numeral.First .. Result.Numeral.Last loop
            C := Text (I);
            if C not in '_' | Digits_First .. Digits_Last
                      | Lower_Ext_Digits_First .. Lower_Ext_Digits_Last
                      | Upper_Ext_Digits_First .. Upper_Ext_Digits_Last
            then
               Error;
            end if;
         end loop;
      end;

      return Result;
   end Parse_Numeric_Literal;

   ----------------------------
   -- Decode_Integer_Literal --
   ----------------------------

   procedure Decode_Integer_Literal
     (Text   : Text_Type;
      Result : out GNATCOLL.GMP.Integers.Big_Integer)
   is
      use GNATCOLL.GMP;
      use GNATCOLL.GMP.Integers;

      function Slice (SS : String_Slice) return String is
        (Strip_Underscores (Text (SS.First .. SS.Last)));

      Parsed : constant Parsed_Numeric_Literal :=
         Parse_Numeric_Literal (Text);
   begin
      --  Evaluate the numeral part of the literal
      declare
         Numeral : constant String := Slice (Parsed.Numeral);
      begin
         Result.Set (Numeral, Int (Parsed.Base));
      end;

      --  Then evaluate and apply the exponent. For integer literals, negative
      --  exponents are invalid.
      if Parsed.Exponent < 0 then
         Error;

      elsif Parsed.Exponent > 0 then
         declare
            Exponent : GNATCOLL.GMP.Integers.Big_Integer;
         begin
            Exponent.Set (10);
            Exponent.Raise_To_N (Unsigned_Long (Parsed.Exponent));
            Result.Multiply (Exponent);
         end;
      end if;
   end Decode_Integer_Literal;

end Libadalang.Sources;
