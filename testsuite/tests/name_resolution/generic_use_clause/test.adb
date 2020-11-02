procedure Test is
   package Pkg is
      type T is null record;
   end Pkg;

   generic
      use Pkg;
   procedure Foo (X : T);
   pragma Test_Block;

   procedure Foo (X : T) is
   begin
      null;
   end Foo;
   pragma Test_Block;
begin
   null;
end Test;
