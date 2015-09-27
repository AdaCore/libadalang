with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces; use Interfaces;

package body Langkit_Support.Text is

   -------------
   -- To_Text --
   -------------

   function To_Text (S : String) return Text_Type is
      Result : Text_Type (1 .. S'Length);
   begin
      for I in Result'Range loop
         declare
            C : constant Character := S (S'First + I - 1);
         begin
            if C in ASCII.NUL .. Character'Val (16#7f#) then
               Result (I) := Wide_Wide_Character'Val (Character'Pos (C));
            else
               raise Constraint_Error with "Invalid ASCII character";
            end if;
         end;
      end loop;
      return Result;
   end To_Text;

   -----------
   -- Image --
   -----------

   function Image (T : Text_Type; With_Quotes : Boolean := False) return String
   is
      subtype Hex_Byte is String (1 .. 2);
      --  Couple of hexadecimal digits, used to represent a byte

      function Byte_Image (B : Unsigned_8) return Hex_Byte;
      --  Given a byte, return the corresponding two-chars hexadecimal image

      ----------------
      -- Byte_Image --
      ----------------

      function Byte_Image (B : Unsigned_8) return Hex_Byte
      is
         type Digits_Type is array (Unsigned_8 range 0 .. 15) of Character;
         D : constant Digits_Type := "0123456789abcdef";
      begin
         return D (B / 16) & D (B mod 16);
      end Byte_Image;

      Result : Unbounded_String;
      W      : Unsigned_32;
   begin
      if With_Quotes then
         Append (Result, '"');
      end if;

      for C of T loop

         --  Determine how to output each character:
         --
         --    - Escape backslashes and escape quotes if With_Quotes.
         --    - Output other ASCII chars as-is.
         --    - Escape non-ASCII small chars with \xXX sequences.
         --    - Escape other medium chars with \uXXXX sequences.
         --    - Escape the rest with \UXXXXXXXX sequences.

         W := Wide_Wide_Character'Pos (C);
         if (With_Quotes and then C = '"') or else C = '\' then
            Append (Result, '\');
            Append (Result, Character'Val (W));
         elsif 16#20# <= W and then W <= 16#7f# then
            Append (Result, Character'Val (W));
         elsif W <= 16#ff# then
            Append (Result, "\x" & Byte_Image (Unsigned_8 (W)));
         elsif W <= 16#ffff# then
            Append
              (Result,
               "\u"
               & Byte_Image (Unsigned_8 (W / 16#100#))
               & Byte_Image (Unsigned_8 (W mod 2#100#)));
         else
            Append
              (Result,
               "\U"
               & Byte_Image (Unsigned_8 (W / 16#100_0000#))
               & Byte_Image (Unsigned_8 (W / 16#1_0000# mod 2#100#))
               & Byte_Image (Unsigned_8 (W / 16#100# mod 2#100#))
               & Byte_Image (Unsigned_8 (W mod 2#100#)));
         end if;
      end loop;

      if With_Quotes then
         Append (Result, '"');
      end if;
      return To_String (Result);
   end Image;

end Langkit_Support.Text;
