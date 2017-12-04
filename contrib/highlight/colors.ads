with Highlighter;

package Colors is

   type Color_Level is mod 2 ** 8;

   type Color_Type is record
      Red, Green, Blue : Color_Level;
   end record;

   type Token_Style is record
      Color : Color_Type;
      Bold  : Boolean;
   end record;
   --  Graphical attributes for an highlighting type

   type Token_Styles is array (Highlighter.Highlight_Type) of Token_Style;

   type Style_Type is record
      Background_Color  : Color_Type;
      Selected_Bg_Color : Color_Type;
      Tok_Styles        : Token_Styles;
   end record;
   --  Set of graphical attributes for syntax highlighting

   package Default_Style is
      use Highlighter;

      Bg_Color              : constant Color_Type := (8, 8, 8);
      Selected_Bg_Color     : constant Color_Type := (36, 36, 36);
      Text_Color            : constant Color_Type := (248, 248, 242);
      Comment_Color         : constant Color_Type := (117, 113, 105);
      Keyword_Color         : constant Color_Type := (255, 95, 135);
      Keyword_Special_Color : constant Color_Type := (224, 128, 32);
      Preprocessor_Color    : constant Color_Type := (215, 255, 215);
      Number_Color          : constant Color_Type := (175, 95, 255);
      String_Color          : constant Color_Type := (230, 219, 116);
      Label_Color           : constant Color_Type := (255, 250, 15);
      Block_Color           : constant Color_Type := (138, 226, 52);
      Type_Color            : constant Color_Type := (102, 217, 239);
      Attribute_Ref_Color   : constant Color_Type := (232, 70, 109);

      Style : constant Style_Type :=
        (Background_Color  => Bg_Color,
         Selected_Bg_Color => Selected_Bg_Color,
         Tok_Styles        =>
           (Text                   => (Text_Color, False),
            Comment                => (Comment_Color, False),
            Keyword                => (Keyword_Color, True),
            Keyword_Type           => (Type_Color, False),
            Keyword_Special        => (Keyword_Special_Color, False),
            Punctuation            => (Text_Color, False),
            Punctuation_Special    => (Type_Color, False),
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

end Colors;
