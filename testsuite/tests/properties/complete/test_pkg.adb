package body Test_Pkg is
   package Foo is
      procedure Bar;
   end Foo;

   function My_Body_Method (Self : A) return Integer;

   function My_Method (Self : A) return Integer is
   begin
      return Self.My_Body_Method;
   end My_Method;

   function My_Body_Method (Self : A) return Integer is
      pragma Unreferenced (Self);
   begin
      Foo.Bar;
      return 15;
   end My_Body_Method;

   package body Foo is separate;
end Test_Pkg;
