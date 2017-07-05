package body Foo is

   I : Integer;

   package Pkg is
      procedure Baz;
   end Pkg;

   package body Pkg is separate;

   procedure Bar is
   begin
      pragma Test (Pkg.Baz);
   end Bar;

end Foo;
