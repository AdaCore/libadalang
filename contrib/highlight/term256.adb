with Langkit_Support.Slocs;
use type Langkit_Support.Slocs.Line_Number;
use type Langkit_Support.Slocs.Column_Number;
with Langkit_Support.Text;

package body Term256 is

   package Slocs renames Langkit_Support.Slocs;

   use type LAL.Token_Type;

   ANSI_Reset : constant String := ASCII.ESC & "[0m";

   -------------------
   -- Style_To_ANSI --
   -------------------

   function Style_To_ANSI (Style : Colors.Token_Style) return String is
      Red   : constant Natural := Natural (Style.Color.Red) * 5 / 255;
      Green : constant Natural := Natural (Style.Color.Green) * 5 / 255;
      Blue  : constant Natural := Natural (Style.Color.Blue) * 5 / 255;

      Bold_Marker         : constant String :=
        (if Style.Bold then "1;" else "");
      Color_Number        : constant Natural :=
        16 + 36 * Red + 6 * Green + Blue;
      Padded_Color_Number : constant String := Natural'Image (Color_Number);
   begin
      return (ASCII.ESC & "[" & Bold_Marker & "38;5;"
              & Padded_Color_Number
                (Padded_Color_Number'First + 1 .. Padded_Color_Number'Last)
              & "m");
   end Style_To_ANSI;

   ----------------
   -- Put_Tokens --
   ----------------

   procedure Put_Tokens
     (Unit       : LAL.Analysis_Unit;
      Highlights : Highlighter.Highlights_Holder;
      S          : Colors.Style_Type;
      Charset    : String)
   is
      --  TODO: use Charset to properly encode token text (see Escape below)

      pragma Unreferenced (Charset);

      Token       : LAL.Token_Type := LAL.First_Token (Unit);
      Last_Sloc   : Slocs.Source_Location := (1, 1);
   begin
      while Token /= LAL.No_Token loop
         declare
            TD         : constant LAL.Token_Data_Type := LAL.Data (Token);
            HL         : constant Highlighter.Highlight_Type :=
              Highlighter.Get (Highlights, TD);
            Sloc_Range : constant Slocs.Source_Location_Range :=
              LAL.Sloc_Range (TD);
            Text       : constant Langkit_Support.Text.Text_Type :=
              LAL.Text (Token);

            function Escape (T : Langkit_Support.Text.Text_Type) return String
            is (Langkit_Support.Text.Image (T));

         begin
            while Last_Sloc.Line < Sloc_Range.Start_Line loop
               Put ((1 => ASCII.LF));
               Last_Sloc.Line := Last_Sloc.Line + 1;
               Last_Sloc.Column := 1;
            end loop;
            while Last_Sloc.Column < Sloc_Range.Start_Column loop
               Put (" ");
               Last_Sloc.Column := Last_Sloc.Column + 1;
            end loop;

            Put
              (Style_To_ANSI (S.Tok_Styles (HL)) & Escape (Text) & ANSI_Reset);
            Last_Sloc := Slocs.End_Sloc (Sloc_Range);
         end;
         Token := LAL.Next (Token);
      end loop;
   end Put_Tokens;

end Term256;
