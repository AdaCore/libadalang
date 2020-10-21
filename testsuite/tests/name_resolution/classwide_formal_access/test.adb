procedure Test is
   package Lol is
      type A is tagged null record;

      procedure Foo (Self : access A'Class) is null;

      type B is new A with null record;
      type C is access all B;
   end Lol;
   use Lol;

   Inst : C := new B'(null record);
begin
   Foo (Inst);
   pragma Test_Statement;
end Test;
