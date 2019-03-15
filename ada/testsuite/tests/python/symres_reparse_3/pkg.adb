package body Pkg is

   procedure Foo (X : access T'Class) is
   begin
      null;
   end Foo;

   procedure Bar (X : access T'Class) is
   begin
      X.Foo;
   end Bar;

end Pkg;
