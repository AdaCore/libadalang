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
         subtype Valid_Codepoint is Unsigned_32
            range Wide_Wide_Character'Pos (Wide_Wide_Character'First)
               .. Wide_Wide_Character'Pos (Wide_Wide_Character'Last);
         Codepoint : Unsigned_32 := 0;
      begin
         for C of Pattern (Pattern'First + 2 .. Pattern'Last - 2) loop
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

end Libadalang.Sources;
