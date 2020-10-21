package Pack is
   function F return Natural;

   type Ints is array (Natural range <>) of Integer;
   type Bounded_Ints (Max_Size : Natural) is record
      Length : Natural := 0;
      Objs   : Ints (1 .. Max_Size);
   end record;

   type Ints_Doubled is array (1 .. 2) of Bounded_Ints (F);
end Pack;
pragma Test_Block;
