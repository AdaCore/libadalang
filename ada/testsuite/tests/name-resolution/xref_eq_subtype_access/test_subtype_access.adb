procedure Test_Subtype_Access is
   type Bit is range 0 .. 1;
   type Bit_Access is access all Bit;
   subtype Sub is Bit_Access;

   S : Sub;

   B : Bit;
begin
   B := 1;
   pragma Test_Statement;

   S.all := B;
   pragma Test_Statement;

   S.all := 0;
   pragma Test_Statement;
end Test_Subtype_Access;
