--  Check basic usage of the Libadalang.Data_Decomposition API, working on
--  simple types.

package Pkg is
   type E1 is (A, B, C, D);

   type I1 is range -256 .. 255;

   type M1 is mod 2 ** 15;
   type M2 is mod 2 ** 32;

   type R1 is digits 7;
   type R2 is digits 7 range 0.0 .. 1.0;
   type R3 is new Float;

   type F1 is delta 0.1 range -1.0 .. 1.0;
   type F2 is delta 0.1 range -1.0 .. 1.0 with Small => 0.03333333;
   type F3 is delta 0.1 range -1.0 .. 1.0 with Small => 1.0 / 30.0;
   type F4 is delta 0.1 digits 14;

   type Rec1 is record
      B : Boolean;
      C : Character;
      I : Integer;
   end record;

   type Rec2 is null record;

   type Bool_Array is array (Positive range <>) of Boolean;
   subtype Bool_Couple is Bool_Array (1 .. 2);
end Pkg;
