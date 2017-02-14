with Ada.Strings.Unbounded;

with Langkit_Support.Slocs;
use type Langkit_Support.Slocs.Line_Number;
use type Langkit_Support.Slocs.Column_Number;
with Langkit_Support.Text;

--  with GNATCOLL.Iconv;

package body HTML is

   package Slocs renames Langkit_Support.Slocs;

   use type LAL.Token_Type;

   Hex_Digits : constant
     array (Colors.Color_Level range 0 .. 15) of Character :=
     "0123456789abcdef";

   ------------
   -- Escape --
   ------------

   function Escape (S : String) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for C of S loop
         case C is
            when '"'    => Append (Result, "&quot;");
            when '&'    => Append (Result, "&amp;");
            when '<'    => Append (Result, "&lt;");
            when '>'    => Append (Result, "&gt;");
            when others => Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Escape;

   -------------------
   -- Color_To_HTML --
   -------------------

   function Color_To_HTML (Color : Colors.Color_Type) return HTML_Color is
      use type Colors.Color_Level;
      Result : HTML_Color;
   begin
      Result (1) := Hex_Digits (Color.Red / 16);
      Result (2) := Hex_Digits (Color.Red mod 16);
      Result (3) := Hex_Digits (Color.Green / 16);
      Result (4) := Hex_Digits (Color.Green mod 16);
      Result (5) := Hex_Digits (Color.Blue / 16);
      Result (6) := Hex_Digits (Color.Blue mod 16);
      return Result;
   end Color_To_HTML;

   -------------------
   -- Put_CSS_Rules --
   -------------------

   procedure Put_CSS_Rules (S : Colors.Style_Type) is
   begin
      Put ("pre.code_highlight { background-color: #"
           & Color_To_HTML (S.Background_Color) & "; }" & ASCII.LF);
      for HL in Highlighter.Highlight_Type'Range loop
         declare
            Style : Colors.Token_Style renames S.Tok_Styles (HL);
         begin
            Put ("pre.code_highlight span."
                 & Highlighter.Highlight_Name (HL) & " {");
            Put (" color: #" & Color_To_HTML (Style.Color) & ";");
            if Style.Bold then
               Put (" font-weight: bold;");
            end if;
            Put (" }" & ASCII.LF);
         end;
      end loop;
   end Put_CSS_Rules;

   ----------------
   -- Put_Tokens --
   ----------------

   procedure Put_Tokens
     (Unit       : LAL.Analysis_Unit;
      Highlights : Highlighter.Highlights_Holder;
      Charset    : String)
   is
      --  TODO: use Charset to properly encode token text (see Escape below)

      pragma Unreferenced (Charset);

      Token       : LAL.Token_Type := LAL.First_Token (Unit);
      Last_Sloc   : Slocs.Source_Location := (1, 1);
   begin
      Put ("<pre class=""code_highlight"">");
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
            is (Escape (Langkit_Support.Text.Image (T)));

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

            Put ("<span class=""" & Highlighter.Highlight_Name (HL) & """>");
            Put (Escape (Text));
            Put ("</span>");
            Last_Sloc := Slocs.End_Sloc (Sloc_Range);
         end;
         Token := LAL.Next (Token);
      end loop;
      Put ("</pre>");
   end Put_Tokens;

end HTML;
