procedure Test_Array_Indices is
   type Color is (Yellow, Blue, Green, Black, White);
   type Shade is (Dark, Light);
   type A is array (Color, Shade) of Integer;

   Inst : A := (Yellow => (others => 11),
                others => (Dark => 12, others => 15));
   pragma Test_Statement;
begin
   null;
end Test_Array_Indices;
