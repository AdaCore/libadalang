procedure Test is
   type A is record
      X : Integer;
      Y : Integer;
   end record;

   type B is record
      X : A;
      Y : A;
   end record;

   type C is record
      X : B;
      Y : B;
   end record;
begin
   declare
      R_A_1 : A := (Y => 1, others => 42);
      R_A_2 : A := (others => 42);

      R_B_1 : B := (X => R_A_1, others => R_A_2);
      R_B_2 : B := (X => R_A_1, others => (others => 42));
      R_B_3 : B := (others => (others => 42));

      R_C_1 : C :=
        (X => (others => (others => 42)),
         others => (others => (others => 42)));

      R_C_2 : C := (others => (others => (others => 42)));
   begin
      null;
   end;
   pragma Test_Block;
end Test;
