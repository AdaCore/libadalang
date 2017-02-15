with Libadalang.Analysis;

with Colors;
with Highlighter;

package Term256 is

   package LAL renames Libadalang.Analysis;

   function Style_To_ANSI (Style : Colors.Token_Style) return String;
   --  Return the ANSI escape sequence to materialize the given token style

   generic
      with procedure Put (S : String);
   procedure Put_Tokens
     (Unit       : LAL.Analysis_Unit;
      Highlights : Highlighter.Highlights_Holder;
      S          : Colors.Style_Type;
      Charset    : String)
     with Pre => Highlighter.Highlights_Match_Unit (Unit, Highlights);
   --  Using Put, write text with ANSI escape sequences to syntax highlight
   --  source code from Unit according to the input highlighting data and the
   --  S highlighting style.

end Term256;
