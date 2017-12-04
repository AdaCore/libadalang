with Ada.Strings.Unbounded;

with Langkit_Support.Text;

--  with GNATCOLL.Iconv;

package body HTML is

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

      function Escape (T : Langkit_Support.Text.Text_Type) return String
      is (Escape (Langkit_Support.Text.Image (T)));

      procedure Put_Token
        (Token : LAL.Token_Type;
         Data  : LAL.Token_Data_Type;
         HL   : Highlighter.Highlight_Type);
      procedure New_Line;
      procedure Indent (Length : Natural);
      --  Generic parameters for Put_Tokens below

      ---------------
      -- Put_Token --
      ---------------

      procedure Put_Token
        (Token : LAL.Token_Type;
         Data  : LAL.Token_Data_Type;
         HL    : Highlighter.Highlight_Type)
      is
         pragma Unreferenced (Data);
         Text : constant Langkit_Support.Text.Text_Type := LAL.Text (Token);
      begin
         Put ("<span class=""" & Highlighter.Highlight_Name (HL) & """>");
         Put (Escape (Text));
         Put ("</span>");
      end Put_Token;

      --------------
      -- New_Line --
      --------------

      procedure New_Line is
      begin
         Put ((1 => ASCII.LF));
      end New_Line;

      ------------
      -- Indent --
      ------------

      procedure Indent (Length : Natural) is
      begin
         Put ((1 .. Length => ' '));
      end Indent;

      procedure Put_Tokens is new Highlighter.Put_Tokens;
   begin
      Put ("<pre class=""code_highlight"">");
      Put_Tokens (Unit, Highlights);
      Put ("</pre>");
   end Put_Tokens;

end HTML;
