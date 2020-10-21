procedure Test is
   type Sequence is array (Natural range <>) of Integer;

   function Foo (S1, S2 : Sequence) return Boolean is
   begin
      return
        (for all K in S1'Range => S1(K) > 0) and then
        (for some E of S2 => E < 0);
      pragma Test_Statement;
   end Foo;
begin
   null;
end Test;
