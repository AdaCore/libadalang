procedure Repro_Multidim is
   subtype R is Integer range 1 .. 12;
   type A is array (R, R) of Natural;

   type Rec is record
      B : Integer;
      Arr : A;
   end record;

   Inst : Rec;

begin
   Inst := (B => 12, Arr => (others => (others => 1)));
   pragma Test_Statement;
end Repro_Multidim;
