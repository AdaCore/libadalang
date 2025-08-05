procedure Test is
   generic
      type T is range <>;
   package Pkg is
      X : T'Base := T'First;

      Y : T := X * X;

      subtype U is T;

      type V is new Integer;
   end Pkg;

   package My_Pkg is new Pkg (Positive);
   pragma Test_Statement;
begin
   My_Pkg.X := 2;

   My_Pkg.X := My_Pkg.U'Succ (My_Pkg.X);
   pragma Test_Statement;

   declare
      subtype V is My_Pkg.V;
      Y : V := V'Succ (3);
      pragma Test_Statement;
   begin
      null;
   end;
end Test;
