procedure Test is
   generic
      type E is (<>);
      type T is array (E) of Boolean;
   package P1 is
      procedure X (A : T);
   end P1;

   package body P1 is
      procedure X (A : T) is
      begin
         null;
      end X;
   end P1;


   generic
      type E is (<>);
      type T is array (E) of Boolean;
   package P2 is
   end P2;

   package body P2 is
      subtype ST is P2.T;
      package I is new P1 (E, ST);

      procedure X (A : ST) renames I.X;
      pragma Test_Statement;
   end P2;
begin
   null;
end Test;
