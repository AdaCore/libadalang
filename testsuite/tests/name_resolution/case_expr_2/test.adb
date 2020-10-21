procedure Test is
   type T is (A, B, C);

   X : T;
   Y : Integer;
begin
   Y := (case X is
         when T range A .. B => 1,
         when others => 2);
   pragma Test_Statement;
end Test;
