procedure Arrays is
   type Color is (Red, Green, Blue);
   type Int_Array is array (Integer range <>) of Integer;
   type Color_Array is array (Color) of Integer;

   type Ref (Data : access Int_Array) is null record
   with Implicit_Dereference => Data;

   A  : Int_Array (5 .. 8) := (others => 0);
   CA : Color_Array := (1, 2, 3);
   R  : constant Ref := (Data => new Int_Array'(5 .. 8 => 0));
begin
   --  The type of Loop_Index is the index type of the array

   for E of A loop
      pragma Assert (A (E'Loop_Index) = E);
      pragma Test_Statement;
   end loop;

   for E of reverse CA loop
      pragma Assert (E'Loop_Index /= Red);
      pragma Test_Statement;
   end loop;

   --  For one-dimensional arrays, Loop_Index is Loop_Index (1)

   for E of A (6 .. 7) loop
      pragma Assert (E'Loop_Index (1) = E'Loop_Index);
      pragma Test_Statement;
   end loop;

   --  Loop_Index sees through the implicit dereference of the iterable

   for E of R loop
      pragma Assert (R (E'Loop_Index) = E);
      pragma Test_Statement;
   end loop;
end Arrays;
