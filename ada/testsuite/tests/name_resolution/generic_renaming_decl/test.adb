procedure Test is
   generic
      type T is private;
   procedure G1 (X : T);

   generic procedure G2 renames G1;
   pragma Test_Statement;


   generic
      type T is private;
   package P1 is
   end P1;

   generic package P2 renames P1;
   pragma Test_Statement;
begin
   null;
end Test;
