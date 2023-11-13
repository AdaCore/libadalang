procedure Test is
   package Pkg is
      type T is null record;
      procedure P (B : T);
   end Pkg;

   package body Pkg is
      procedure P (B : T) is null;

      procedure Q (B : T) is
      begin
         B.P;
         pragma Test_Statement;
      end Q;
   end Pkg;

   package Pkg2 is
      type T is null record;
      subtype U is T;
      procedure P (B : U);
   end Pkg2;

   package body Pkg2 is
      procedure P (B : U) is null;

      procedure Q (B : U) is
      begin
         B.P;
         pragma Test_Statement;
      end Q;
   end Pkg2;
begin
   null;
end Test;
