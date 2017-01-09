with Ada.Wide_Wide_Characters.Handling;

with Interfaces; use Interfaces;

package body Libadalang.Sources is

   ---------------------
   -- Decode_Brackets --
   ---------------------

   function Decode_Brackets (Pattern : Text_Type) return Wide_Wide_Character is
   begin
      if Pattern'Length < 6 or else Pattern'Length > 12
         or else Pattern (Pattern'First .. Pattern'First + 1) /= "["""
         or else Pattern (Pattern'Last - 1 .. Pattern'Last) /= """]"
      then
         raise Invalid_Brackets_Encoding;
      end if;

      declare
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
                  raise Invalid_Brackets_Encoding;
               end if;
               Codepoint := 16 * Codepoint + Digit;
            end;
         end loop;
         return Wide_Wide_Character'Val (Codepoint);
      end;
   end Decode_Brackets;

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize (Name : Text_Type) return Text_Type is
      Result      : Text_Type (Name'Range);
      Result_Last : Integer := Name'First - 1;

      I : Positive := Name'First;
   begin
      --  Decode bracket encodings

      while I <= Name'Last loop
         Result_Last := Result_Last + 1;
         declare
            C : constant Wide_Wide_Character := Name (I);
            J : Positive := I + 1;
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
               Result (Result_Last) := Decode_Brackets (Name (I .. J));
               I := J;

            else
               --  Otherwise, just copy the chararcters

               Result (Result_Last) := C;
            end if;
         end;
         I := I + 1;
      end loop;

      --  Perform case folding: always convert to lower case

      declare
         Result_Slice : Text_Type renames Result (Result'First .. Result_Last);
      begin
         return Ada.Wide_Wide_Characters.Handling.To_Lower (Result_Slice);
      end;
   end Canonicalize;

end Libadalang.Sources;
