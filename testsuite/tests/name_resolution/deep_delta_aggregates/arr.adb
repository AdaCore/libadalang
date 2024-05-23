procedure Arr is

   type R is record
      F, G, H : Integer;
   end record;

   type My_Arr is array (Positive range <>) of R;

   procedure Test (X : in out My_Arr; I, J, K, L : Positive);

   procedure Test (X : in out My_Arr; I, J, K, L : Positive) is
   begin
      X := (X with delta
           (I).F => 1,
           (J).G => 2,
           (K).F => 3,
           (L).H => 4);
      pragma Test_Statement;

      X := ((if True then X else X) with delta (I).F => 4);
      pragma Test_Statement;
   end Test;

begin
   null;
end Arr;
