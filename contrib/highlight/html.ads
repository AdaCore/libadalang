with Libadalang.Analysis;

with Highlighter;

package HTML is

   package LAL renames Libadalang.Analysis;

   function Escape (S : String) return String;
   --  Escape all special characters in S so that it can be included as-is as
   --  text in an HTML document.

   type Color_Level is mod 2 ** 8;

   type Color_Type is record
      Red, Green, Blue : Color_Level;
   end record;

   subtype HTML_Color is String (1 .. 6);
   --  6-digit hexadecimal representation for an HTML color. Just prepend a '#'
   --  character to get a valid color.

   function Color_To_HTML (Color : Color_Type) return HTML_Color;

   type Token_Style is record
      Color : Color_Type;
      Bold  : Boolean;
   end record;
   --  Graphical attributes for an highlighting type

   type Token_Styles is array (Highlighter.Highlight_Type) of Token_Style;

   type Style_Type is record
      Background_Color : Color_Type;
      Tok_Styles       : Token_Styles;
   end record;
   --  Set of graphical attributes for syntax highlighting

   package Default_Style is
      use Highlighter;

      Bg_Color            : constant Color_Type := (8, 8, 8);
      Text_Color          : constant Color_Type := (248, 248, 242);
      Comment_Color       : constant Color_Type := (117, 113, 94);
      Keyword_Color       : constant Color_Type := (255, 95, 135);
      Preprocessor_Color  : constant Color_Type := (215, 255, 215);
      Number_Color        : constant Color_Type := (175, 95, 255);
      String_Color        : constant Color_Type := (230, 219, 116);
      Label_Color         : constant Color_Type := (255, 250, 15);
      Block_Color         : constant Color_Type := (138, 226, 52);
      Type_Color          : constant Color_Type := (102, 217, 239);
      Attribute_Ref_Color : constant Color_Type := (232, 70, 109);

      Style : constant Style_Type :=
        (Background_Color => Bg_Color,
         Tok_Styles       =>
           (Text                   => (Text_Color, False),
            Comment                => (Comment_Color, False),
            Keyword                => (Keyword_Color, True),
            Punctuation            => (Text_Color, False),
            Operator               => (Keyword_Color, False),
            Preprocessor_Directive => (Preprocessor_Color, False),
            Integer_Literal        => (Number_Color, False),
            String_Literal         => (String_Color, False),
            Character_Literal      => (String_Color, False),
            Identifier             => (Text_Color, False),
            Label_Name             => (Label_Color, False),
            Block_Name             => (Block_Color, False),
            Type_Name              => (Type_Color, False),
            Attribute_Name         => (Attribute_Ref_Color, False)));
   end Default_Style;

   generic
      with procedure Put (HTML : String);
   procedure Put_CSS_Rules (S : Style_Type);
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
