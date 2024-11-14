procedure Test is
   package Pkg is
      function Foo return Integer;
   end Pkg;

   package body Pkg is
      function Foo return Integer is (1);
   end Pkg;
begin
   null;
end Test;
