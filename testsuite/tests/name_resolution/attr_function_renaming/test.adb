procedure Test is
   function Foo (X : Integer) return String renames Integer'Image;
   pragma Test_Statement;

   generic
      with procedure Bar (X : Integer) return String is Integer'Image;
      pragma Test_Block;
   package Test is
   end Test;
begin
   null;
end Test;
