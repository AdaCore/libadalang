procedure Test_Scalar is
   package Pkg is
      type Enum_T is (A, B, C);
      type Int_T is range 1 .. 10;
      type Mod_T is mod 3;
      type Fixed_T is delta 0.1 range 0.0 .. 10.0;
      type Float_T is digits 8;

      procedure Foo (Self : Enum_T) is null;
      procedure Foo (Self : Int_T) is null;
      procedure Foo (Self : Mod_T) is null;
      procedure Foo (Self : Fixed_T) is null;
      procedure Foo (Self : Float_T) is null;
   end Pkg;

   use Pkg;

   X_1 : Enum_T;
   X_2 : Int_T;
   X_3 : Mod_T;
   X_4 : Fixed_T;
   X_5 : Float_T;
begin
   X_1.Foo;
   pragma Test_Statement;
   X_2.Foo;
   pragma Test_Statement;
   X_3.Foo;
   pragma Test_Statement;
   X_4.Foo;
   pragma Test_Statement;
   X_5.Foo;
   pragma Test_Statement;
end Test_Scalar;

