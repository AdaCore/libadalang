procedure Test is
   generic
      type T is private;
   package Pkg is
      procedure Bar;
   end Pkg;

   generic
      with procedure Foo (X : Integer);
   package Internal is
      procedure Bar;
   end Internal;

   procedure Act (X : Integer) is null;

   package body pkg is
      package Internal_Pkg is new Internal (Foo => Act);

      procedure Bar is
      begin
         Internal_Pkg.Bar;
      end Bar;
   end Pkg;

   package body Internal is
      procedure Bar is
      begin
         Foo (12);
      end Bar;
   end Internal;

   package My_Pkg is new Pkg (Integer);
begin
   My_Pkg.Bar;
end Test;
