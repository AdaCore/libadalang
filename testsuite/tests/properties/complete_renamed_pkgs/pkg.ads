package Pkg is
   procedure Foo is null;

   generic
   package Gen is
      procedure Bar;
   end Gen;
end Pkg;
