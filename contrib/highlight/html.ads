with Libadalang.Analysis;

with Colors;
with Highlighter;

package HTML is

   package LAL renames Libadalang.Analysis;

   function Escape (S : String) return String;
   --  Escape all special characters in S so that it can be included as-is as
   --  text in an HTML document.

   subtype HTML_Color is String (1 .. 6);
   --  6-digit hexadecimal representation for an HTML color. Just prepend a '#'
   --  character to get a valid color.

   function Color_To_HTML (Color : Colors.Color_Type) return HTML_Color;

   generic
      with procedure Put (HTML : String);
   procedure Put_CSS_Rules (S : Colors.Style_Type);
   --  Using Put, write the CSS rules to implement the given syntax
   --  highlighting style.

   generic
      with procedure Put (HTML : String);
   procedure Put_Tokens
     (Unit       : LAL.Analysis_Unit;
      Highlights : Highlighter.Highlights_Holder;
      Charset    : String)
     with Pre => Highlighter.Highlights_Match_Unit (Unit, Highlights);
   --  Using Put, write HTML content to syntax highlight source code from Unit
   --  according to the input highlighting data.

end HTML;
