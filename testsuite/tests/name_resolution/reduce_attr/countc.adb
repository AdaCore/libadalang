procedure Countc is

   function Acc(Counter : Integer; Char : Character) return Integer is
     (if Char = 'c' then Counter + 1 else Counter);

   Buffer : String := "abcdefedcba";
   Count1, Count2 : Integer := 0;
begin
   Count1 := Buffer'Reduce(Acc, Count1);
   pragma Test_Statement;

   Count2 := [for C of Buffer => C]'Reduce(Acc, Count2);
   pragma Test_Statement;
end Countc;
