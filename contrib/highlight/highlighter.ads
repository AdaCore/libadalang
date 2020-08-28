with Langkit_Support.Token_Data_Handlers;

with Libadalang.Analysis;
with Libadalang.Common;

package Highlighter is

   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   subtype Token_Index is Langkit_Support.Token_Data_Handlers.Token_Index;

   type Highlight_Type is
     (Text,
      Comment,
      Keyword,
      Keyword_Type,
      Keyword_Special,
      Punctuation,
      Punctuation_Special,
      Operator,
      Preprocessor_Directive,
      Integer_Literal,
      String_Literal,
      Character_Literal,
      Identifier,
      Label_Name,
      Block_Name,
      Type_Name,
      Attribute_Name);
   --  Highlighting "category": keyword, comment, identifier, ...

   function Highlight_Name (H : Highlight_Type) return String is
     (Highlight_Type'Image (H));
   --  Return an unique name for a given highlighting type

   type Highlights_Holder (Token_Count, Trivia_Count : Token_Index) is
   limited private;
   --  Annotations on a set of tokens. Provides highlighting types for all
   --  tokens/trivias.

   function Get
     (Highlights : Highlights_Holder;
      Token      : LALCO.Token_Data_Type) return Highlight_Type;
   --  Return the annotation in Highlights corresponding to Token

   procedure Set
     (Highlights : in out Highlights_Holder;
      Token      : LALCO.Token_Data_Type;
      HL         : Highlight_Type);
   --  Assign the HL highlighting type to Token

   procedure Set_Range
     (Highlights  : in out Highlights_Holder;
      First, Last : LALCO.Token_Reference;
      HL          : Highlight_Type);
   --  Likewise, for a range of tokens

   function Highlights_Match_Unit
     (Unit       : LAL.Analysis_Unit;
      Highlights : Highlights_Holder) return Boolean
   is (Natural (Highlights.Token_Count) = LAL.Token_Count (Unit)
       and then
       Natural (Highlights.Trivia_Count) = LAL.Trivia_Count (Unit));
   --  Return whether Highlights has the appropriate number of tokens/trivias
   --  to annotate the tokens in Unit.

   procedure Highlight
     (Unit       : LAL.Analysis_Unit;
      Highlights : in out Highlights_Holder)
     with Pre => Highlights_Match_Unit (Unit, Highlights);
   --  Compute highlighting types for all tokens in Unit

   generic
      with procedure Put_Token
        (Token : LALCO.Token_Reference;
         Data  : LALCO.Token_Data_Type;
         HL    : Highlight_Type) is <>;
      with procedure New_Line is <>;
      with procedure Add_Whitespace (C : Character) is <>;
   procedure Put_Tokens
     (Unit       : LAL.Analysis_Unit;
      Highlights : Highlights_Holder)
     with Pre => Highlights_Match_Unit (Unit, Highlights);
   --  Using the given output primitives, output the token stream from Unit
   --  with the given highlightings.

private

   type Highlight_Array is array (Token_Index range <>) of Highlight_Type;

   type Highlights_Holder (Token_Count, Trivia_Count : Token_Index) is
   limited record
      Token_Highlights  : Highlight_Array (1 .. Token_Count);
      Trivia_Highlights : Highlight_Array (1 .. Trivia_Count);
   end record;

end Highlighter;
