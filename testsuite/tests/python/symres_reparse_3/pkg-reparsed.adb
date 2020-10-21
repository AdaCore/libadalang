package body Pkg is

   procedure Bar (X : access T'Class) is
   begin
      X.Foo;
   end Bar;

end Pkg;
