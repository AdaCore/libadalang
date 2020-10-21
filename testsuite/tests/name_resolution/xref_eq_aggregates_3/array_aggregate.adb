procedure Array_Aggregate is
   type Integer is range 1 .. 1000;
   type A is array (Integer) of Integer;
   Inst : A;

   I : Integer := 30;
begin
   Inst := (others => 12);
   pragma Test_Statement;

   Inst := (1 | 2 | 3 => I, others => 12);
   pragma Test_Statement;
end Array_Aggregate;
