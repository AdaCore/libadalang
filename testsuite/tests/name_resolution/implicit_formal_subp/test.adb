procedure Test is
   generic
      with procedure Foo is <>;
   package Pkg is
      procedure Bar;
   end Pkg;

   package body Pkg is
      procedure Bar is
      begin
         Foo;
      end Bar;
   end Pkg;

   procedure Foo is null;
   package My_Pkg is new Pkg;
   pragma Test_Statement;
begin
   null;
end Test;

