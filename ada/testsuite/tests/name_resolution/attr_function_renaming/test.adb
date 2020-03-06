procedure Test is
   function Foo (X : Integer) return String renames Integer'Image;
   pragma Test_Statement;
begin
   null;
end Test;
