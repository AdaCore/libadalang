procedure Main is
   package Pkg is
      E : exception;

      procedure Test;
   end Pkg;
   package body Pkg is

      procedure Test is
      begin
         raise E;
      end Test;
   end Pkg;
begin
   null;
end Main;
