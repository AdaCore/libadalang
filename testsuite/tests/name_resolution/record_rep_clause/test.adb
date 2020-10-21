package Test is
   type R is record
      A : Integer;
      B : Boolean;
   end record;

   for R use record
      A at 0 range 0 .. 31;
      B at 1 range 32 .. 32;
   end record;
end Test;
pragma Test_Block;
