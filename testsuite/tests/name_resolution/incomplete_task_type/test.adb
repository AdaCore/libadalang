package body Test is
   task body T is
      type S;
      type P is access S;
      type S is null record;

      procedure H (X : P) is
         TS : P := X;
         pragma Test_Statement;
      begin
         null;
      end H;
   begin
      null;
   end T;
end Test;

