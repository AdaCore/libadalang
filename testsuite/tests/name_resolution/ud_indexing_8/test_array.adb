with Ada.Containers.Vectors;

procedure Test_Array is
   package P is
      type Style_Enum_Type is
        (None,
         Keyword,
         String_Style,
         Number,
         Comment,
         Block,
         Type_Style,
         Aspect,
         Aspect_Keyword,
         Aspect_String,
         Aspect_Number,
         Aspect_Comment,
         Aspect_Block,
         Aspect_Type);

      subtype Syntax_Style_Enum_Type is
        Style_Enum_Type range Block .. Type_Style;

      subtype Token_Index is Natural;

      package Style_Vectors is new
        Ada.Containers.Vectors
          (Index_Type   => Token_Index,
           Element_Type => Style_Enum_Type);

      Vector : Style_Vectors.Vector;

      Map : array (Syntax_Style_Enum_Type) of Style_Enum_Type :=
        (Block => None, Type_Style => None);
   end P;

   use P;
   I : Token_Index := 1;
   E : Style_Enum_Type;
begin
   E := Map (Vector (I));
   pragma Test_Statement;

   Vector (I) := Map (Vector (I));
   pragma Test_Statement;
end;
