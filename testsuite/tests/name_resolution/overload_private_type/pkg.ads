package Pkg is
   type Array_T is array (Positive range <>) of Float;
   type Record_T is private;
private
   type Record_T is record
      X, Y : Integer;
   end record;
end Pkg;
