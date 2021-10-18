procedure Test is
   package Pkg is
      Error : exception;
   end Pkg;

   procedure Foo is
   begin
   exception
      when Pkg.Error =>
         null;
   end Test;
   pragma Test_Block;
begin
   null;
end Test;
