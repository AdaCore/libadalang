with Foo;

procedure Test is
   generic
      with function Hash (X : Integer) return Integer;
   package Pkg is
   end Pkg;

   use Foo;

   package My_Pkg is new Pkg (Hash);
   pragma Test_Statement;
begin
   null;
end Test;
