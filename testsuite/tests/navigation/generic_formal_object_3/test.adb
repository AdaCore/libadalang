procedure Test is
   generic
      X, Y : Integer;
   package Pkg is
      V : Integer := X;
      W : Integer := Y;
   end Pkg;

   package My_Pkg is new Pkg (12, 13);
begin
   null;
end Test;
