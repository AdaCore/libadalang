procedure Set is
   generic
      type Item_Type is private;
      type Item_Count is range <> or use Natural;
      pragma Test_Block;
      I : Item_Count;
   package Pck is
   end Pck;

   package New_Pck1 is new Pck (Item_Type => Integer, I => 1);
   pragma Test_Statement;

   package New_Pck2 is new Pck (Integer, Integer, -1);
   pragma Test_Statement;
begin
   null;
end Set;
