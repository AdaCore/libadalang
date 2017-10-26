procedure Test_Slice is
   function A return String is ("Lol wat");
   function A return Integer is (8);

   B : String := "is that";
   C : String := B & A (3 .. 5);
   pragma Test_Statement;
begin
   null;
end Test_Slice;
