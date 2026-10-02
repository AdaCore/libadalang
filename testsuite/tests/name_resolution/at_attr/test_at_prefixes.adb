procedure Test_At_Prefixes is
   type Rec is record
      A, B : Integer;
   end record;

   type Arr is array (1 .. 3) of Integer;

   R : Rec := (A => 1, B => 2);
   A : Arr := (others => 0);
   Y : Integer;
begin
   <<L1>>
   R.A := 10;
   Y := R.A'At (L1);
   pragma Test_Statement;

   for I in 1 .. 3 loop
      <<L2>>
      A (I) := A (I) + 1;
      Y := A (I)'At (L2);
      pragma Test_Statement;
   end loop;
end Test_At_Prefixes;
