procedure Testifc is
   package Pouet is

   type A is interface;
   procedure Frobulize (Self : A) is null;

   type E is interface;
   procedure Barulize (Self : E) is null;

   type B is new A and E with null record;

   Inst : B;
   end Pouet;
   use Pouet;
begin
   Frobulize (Inst);
   Inst.Frobulize;
end Testifc;
pragma Test_Block;
