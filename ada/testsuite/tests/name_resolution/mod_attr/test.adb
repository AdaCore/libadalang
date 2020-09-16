procedure Test is
   type Mod24 is mod 2 ** 24;

   Time_Dif : Mod24;
   Seconds_Now : Integer := 0;
   Seconds_Nonce : Mod24 := 0;
begin
   Time_Dif := Mod24'Mod (Seconds_Now) - Seconds_Nonce;
   pragma Test_Statement;
end Test;
