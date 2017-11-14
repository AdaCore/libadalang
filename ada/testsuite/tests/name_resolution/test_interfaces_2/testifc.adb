procedure TestIfc is
   package Pkg is
      type A is interface;
      procedure Foo (Self : A; O : Natural) is null;

      type B is interface and A;

      procedure Bar (Self : B) is null;

      type C is new B with null record;
   end Pkg;

   use Pkg;

   Inst : B'Class := C'(null record);
begin
   Foo (Inst, 12);
   pragma Test_Statement;
end Testifc;
