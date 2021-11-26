procedure Test is
   package Pkg is
      type Foo is null record;
   end Pkg;

   use Pkg;

   procedure Bar (X: Foo) is null;
   pragma Test_Block;
begin
   null;
end Test;
