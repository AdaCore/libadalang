procedure Test is
   On_Type : String := Float'Image (2.0);
   pragma Test_Statement;

   My_Float : Float := 2.0;
   On_Expr : String := My_Float'Image;
   pragma Test_Statement;

   On_Qualified_Expr : String := Float'(2.0)'Image;
   pragma Test_Statement;

   Wide_Image : Wide_String := Float'Wide_Image (2.0);
   pragma Test_Statement;

   With_Subscript : String := Float'Image (2.0) (1 .. 2);
   pragma Test_Statement;
begin
   null;
end Test;
