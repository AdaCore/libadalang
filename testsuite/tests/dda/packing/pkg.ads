--  Check the handling of packed records

package Pkg is

   type Bit_Field is array (Positive range <>) of Boolean with Pack;

   type R is record
      X1 : Bit_Field (1 .. 2);
      X2 : Bit_Field (3 .. 4);
      X3 : Bit_Field (5 .. 5);
      X4 : Bit_Field (6 .. 12);
      X5 : Bit_Field (13 .. 16);
   end record with Pack;

end Pkg;
