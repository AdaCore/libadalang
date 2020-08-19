package body Pkg is

   procedure Prim (Value : T);

   procedure Prim (Value : T) is
   begin
      null;
   end Prim;

   procedure Foo is
      V : T;
   begin
      V.Prim;
      pragma Test_Statement;
   end Foo;
end Pkg;
