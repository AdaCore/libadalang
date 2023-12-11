procedure Test is
   type E is (X, Y);

   EX  : constant E       := X;
   C   : constant Natural := 3;

   A_1 : constant Boolean :=
      1 <= (if True then C else 2);
   pragma Test_Statement;

   A_2 : constant Boolean :=
      1 <= (if True then 2 else C);
   pragma Test_Statement;

   A_3 : constant Boolean :=
      1 <= (if True then 2 elsif False then C else 3);
   pragma Test_Statement;

   B_1 : constant Boolean :=
      4 <= (case EX is when X => C, when Y => 2);
   pragma Test_Statement;

   B_2 : constant Boolean :=
      4 <= (case EX is when X => 2, when Y => C);
   pragma Test_Statement;
begin
   null;
end Test;
