procedure Main is
   procedure Foo (X : Integer; Y: Boolean; Z : Boolean);

   generic
      type A is private;
      type B is private;
   package Pkg is
   end Pkg;

   package Inst_1 is new Pkg (Integer, Boolean);
   package Inst_2 is new Pkg (B => Boolean, A => Integer);
begin
   Foo (Y => True, Z => False, X => 3);
   Foo (42, Y => True, Z => False);
   Foo (42, True, False);
end Main;
