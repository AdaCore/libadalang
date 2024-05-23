pragma Extensions_Allowed (All_Extensions);

procedure Test is
   type R is record
      F, G, H : Integer;
   end record;

   type My_Arr is array (Positive range <>) of R;

   type S is record
      A : My_Arr (1 .. 2);
      B : Integer;
   end record;

   A : My_Arr := ((1, 2, 3), (4, 5, 6));

   B : S := (A => A, B => 1);

begin
   A := (A with delta (1) => (0,0,0));
   pragma Test_Statement;

   A := (A with delta (A (1).F) => (1,1,1));
   pragma Test_Statement;

   B := (B with delta A (1).F => 1);
   pragma Test_Statement;

   B := (B with delta A (B.B).F => 1);
   pragma Test_Statement;
end;
