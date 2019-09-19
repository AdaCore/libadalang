procedure Test is
   On_Type : String := Float'Image (2.0);
   pragma Test_Statement;

   My_Float : Float := 2.0;
   On_Expr : String := My_Float'Image;
   pragma Test_Statement;
begin
   null;
end Test;
