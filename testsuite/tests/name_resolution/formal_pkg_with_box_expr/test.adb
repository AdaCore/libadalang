procedure Test is
   generic
      type T is private;
      type U is private;
   package Pkg is
   end Pkg;

   generic
      with package P_Inst is new Pkg (Integer, <>);
   package Foo is
   end Foo;
   pragma Test_Block;
begin
   null;
end Test;
