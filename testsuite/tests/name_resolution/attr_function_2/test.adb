procedure Test is
   X : String := Integer'Image (2);
   pragma Test_Statement;

   generic
      type Param_Type is private;
      type Return_Type (<>) is private;
      with function Foo (X : Param_Type) return Return_Type;
   package Pkg_G is
   end Pkg_G;

   package My_Pkg_Image is new Pkg_G
     (Integer,
      String,
      Integer'Image);
   pragma Test_Statement;

   function Img (X : Integer) return String renames Integer'Image;
   pragma Test_Statement;

   package My_Pkg_Floor is new Pkg_G (Float, Float, Float'Floor);
   pragma Test_Statement;

begin
   null;
end Test;

