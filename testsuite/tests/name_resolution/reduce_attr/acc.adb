procedure Acc is

   type Real is digits 8;
   type Matrix is array(Integer range <>, Integer range <>) of Real;

   type Accumulator is record
      Sum   : Real;
      Count : Integer;
   end record;

   function Accumulate (L, R : Accumulator) return Accumulator is
     (Sum   => L.Sum   + R.Sum,
      Count => L.Count + R.Count);

   function Average_of_Values_Greater_Than_100 (M : Matrix) return Real is
      Acc : constant Accumulator :=
        [for Val of M when Val > 100.0 => (Val, 1)]
           'Reduce(Accumulate, (Sum => 0.0, Count => 0));
      pragma Test_Statement;
   begin
      return Acc.Sum / Real(Acc.Count);
   end Average_of_Values_Greater_Than_100;
begin
   null;
end Acc;
