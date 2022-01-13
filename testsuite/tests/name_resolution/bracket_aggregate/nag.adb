procedure Nag is
   type Matrix is array(Integer range <>, Integer range <>) of Float;
   subtype M23 is Matrix (1..2, 1..3);

   Matrix1 : M23 := ((1.1, 1.2, 1.3), (2.1, 2.2, 2.3));
   pragma Test_Statement;

   Matrix2 : M23 := (1 => [1.1, 1.2, 1.3], 2 => [2.1, 2.2, 2.3]);
   pragma Test_Statement;

   Matrix3 : M23 := [1 => (1 => 1.1, 2 => 1.2, 3 => 1.3), 
                     2 => (1 => 2.1, 2 => 2.2, 3 => 2.3)];
   pragma Test_Statement;

   Empty_Matrix1 : Matrix (1..2, 1..3) := [];
   pragma Test_Statement;

   Empty_Matrix2 : M23 := [];
   pragma Test_Statement;

   Empty_Matrix3 : Matrix := [];
   pragma Test_Statement;
begin
   null;
end Nag;
