--  Check the handling of bit order/scalar storage order

with System;

package Pkg is

   type U3 is mod 2 ** 3;
   type U6 is mod 2 ** 6;
   type U7 is mod 2 ** 7;

   type R1 is record
      X1 : U7;
      X2 : U3;
      X3 : U6;
   end record
      with Bit_Order            => System.Low_Order_First,
           Scalar_Storage_Order => System.Low_Order_First;

   for R1 use record
      X1 at 0 range 0 .. 6;
      X2 at 0 range 7 .. 9;
      X3 at 1 range 2 .. 7;
   end record;

   type R2 is record
      X1 : U7;
      X2 : U3;
      X3 : U6;
   end record
      with Bit_Order            => System.High_Order_First,
           Scalar_Storage_Order => System.High_Order_First;

   for R2 use record
      X1 at 0 range 0 .. 6;
      X2 at 0 range 7 .. 9;
      X3 at 1 range 2 .. 7;
   end record;

end Pkg;
