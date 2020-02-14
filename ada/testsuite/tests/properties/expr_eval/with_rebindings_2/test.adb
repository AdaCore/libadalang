procedure Test is
   generic
      Y : in Integer;
   package P_G is
      type Arr is array (Integer range <>) of Boolean;

      subtype R is Integer range 1 .. Y;
      type R_Arr is new Arr (R);
   end P_G;

   package P is new P_G (Y => 10);

   V : P.R_Arr;
begin
   null;
end Test;
