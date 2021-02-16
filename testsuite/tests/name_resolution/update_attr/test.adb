procedure Test is
   type E is (E_A, E_B, E_C);

   type A is array (Positive range 1 .. 10) of E;

   type B is record
      X : Natural;
      Y : Boolean;
   end record;

   V_A : A;
   V_B : B;
begin
   V_A := V_A'Update (3 .. 5 => E_A, 9 => E_C);
   pragma Test_Statement;
   V_B := V_B'Update (X => 3, Y => True);
   pragma Test_Statement;
end Test;
