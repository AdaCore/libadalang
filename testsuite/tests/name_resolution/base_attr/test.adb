procedure Test is
   type T is new Natural range 0 .. 10;

   Y : T'Base := 3;
   X : T'Base := T'Base (12) + 1;
   pragma Test_Statement;

   Z : Integer := T'Base'Alignment;
   pragma Test_Statement;

   type I_T is range 0 .. 10;
   type I_D_T is new I_T range 0 .. 5;

   X_I : I_T'Base := 10;
   pragma Test_Statement;
   X_I_D : I_D_T'Base := 10;
   pragma Test_Statement;

   type E_T is (A, B, C);
   type E_D_T is new E_T range A .. B;

   X_E : E_T'Base := C;
   pragma Test_Statement;
   X_E_D : E_D_T'Base := C;
   pragma Test_Statement;

   type M_T is mod 10;
   type M_D_T is new M_T range 0 .. 5;

   X_M : M_T'Base := 9;
   pragma Test_Statement;
   X_M_D : M_D_T'Base := 9;
   pragma Test_Statement;

   type FL_T is digits 10;
   type FL_D_T is new FL_T range 0.0 .. 100.0;

   X_FL : FL_T'Base := 1000.0;
   pragma Test_Statement;
   X_FL_D : FL_D_T'Base := 1000.0;
   pragma Test_Statement;

   type FP_T is delta 0.1 range 0.0 .. 10.0;
   type FP_D_T is new FP_T range 0.0 .. 5.0;

   X_FP : FP_T'Base := 10.0;
   pragma Test_Statement;
   X_FP_D : FP_D_T'Base := 10.0;
   pragma Test_Statement;
begin
   null;
end Test;
