procedure Test_Brack_Aggr is
   type Arr is array (Positive range <>) of Integer;
   A : Arr := [1, 2, 3, 4];
   pragma Test_Statement;

   B : Arr := [A with 1 => 2, 3 => 4];
   pragma Test_Statement;

   type Point is record
      X, Y : Integer;
   end record;

   P : Point := (12, 15);

   P2 : Point := (P with delta Y => 18);
   pragma Test_Statement;

   P2 : Point := [P with delta Y => 18];
   pragma Test_Statement;

   It : Arr := (for I in 1 .. 200 => I * 2);
   pragma Test_Statement;
begin
   null;
end Test_Brack_Aggr;
