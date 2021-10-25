procedure Test is
   type Fixed_T is delta 0.1 range -1.0 .. 1.0;
   type Float_T is digits 8 range -100.0 .. 100.0;

   subtype Fixed_ST is Fixed_T delta 0.2;
   pragma Test_Statement;
   subtype Float_ST is Float_T digits 4;
   pragma Test_Statement;
begin
   null;
end Test;
