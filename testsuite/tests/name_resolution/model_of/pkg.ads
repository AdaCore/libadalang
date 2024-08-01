package Pkg is
   type T is private;

   procedure Foo (X : Integer) is null;
   procedure Foo (X : T) is null;
   procedure Foo (X : Boolean) is null;

   package Inner is
      function Bar (X : Integer) return Integer is (X);
      function Bar (X : Boolean) return Boolean is (X);
   end Inner;

   generic
      type F is private;
   package Gen is
      procedure Baz (X : Integer) is null;
      procedure Baz (X : F) is null;
   end Gen;

   generic
      type F is private;
   procedure Gen_Proc_1 (X : Integer);

   generic
      type F is private;
   procedure Gen_Proc_2 (X : F);

private
   type T is null record;
   type Private_Type is null record;

   procedure Private_Proc (X : Integer) is null;
   procedure Private_Proc (X : Private_Type) is null;
end Pkg;
