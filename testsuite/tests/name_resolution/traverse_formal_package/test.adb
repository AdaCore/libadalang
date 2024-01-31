procedure Test is
   generic
      with procedure Foo is <>;
   package Formal_G is
      procedure Bar;
   end Formal_G;

   package body Formal_G is
      procedure Bar is
      begin
         Foo;
      end Bar;
   end Formal_G;

   generic
      with package Formal is new Formal_G (<>);
      pragma Test_Block;
   package Gen is
   end Gen;
begin
   null;
end Test;
