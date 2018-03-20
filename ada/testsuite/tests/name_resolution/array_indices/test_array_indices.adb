procedure Test_Array_Indices is
   type Color is (Yellow, Blue, Green, Black, White);
   type A is array (Color) of Integer;

   Inst : A := (Yellow => 12, others => 15);
begin
   null;
end Test_Array_Indices;
pragma Test_Block;
