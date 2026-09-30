procedure Multidim is
   type Color is (Red, Green, Blue);
   type Small is range 1 .. 10;
   type Matrix is array (Color range <>, Small range <>) of Integer;
   type Row is array (Positive range 1 .. 3) of Integer;
   type Row_Array is array (Color) of Row;

   M  : Matrix (Green .. Blue, 1 .. 2) := (others => (others => 0));
   RA : Row_Array := (others => (others => 0));

   Two : constant := 2;
   One : constant Small := 1;
begin
   --  The type of Loop_Index (N) is the N-th index type. N can be any static
   --  integer expression.

   for E of M loop
      pragma Assert (M (E'Loop_Index (1), E'Loop_Index (2)) = E);
      pragma Test_Statement;
      pragma Assert (E'Loop_Index (Two) = E'Loop_Index (One + 1));
      pragma Test_Statement;

      --  Loop_Index without argument on multidimensional arrays is treated as
      --  Loop_Index (1).

      pragma Assert (E'Loop_Index = Blue);
      pragma Test_Statement;
   end loop;

   --  Arrays of arrays

   for R of RA loop
      for X of R loop
         pragma Assert (RA (R'Loop_Index) (X'Loop_Index) = X);
         pragma Test_Statement;
      end loop;
   end loop;
end Multidim;
