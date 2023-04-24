procedure Test is
   package P is
      type U is tagged record
         B : Boolean;
      end record;

      type T is access all U'Class;

      procedure P1 (X : T) is null;
      procedure P2 (X, Y : T) is null;
      procedure P3 (X, Y : T; Z : Boolean) is null;

      procedure P4 (X : access U'Class := null) is null;
      procedure P5 (X, Y : access U'Class := null) is null;
      procedure P6 (X, Y : access U'Class := null; Z : Boolean := False)
      is null;
   end P;

   package body P is
   end P;

   use P;

   O : T;
begin
   P1 (O);
   pragma Test_Statement;
   P2 (O, O);
   pragma Test_Statement;
   P3 (O, O, True);
   pragma Test_Statement;
   O.P4;
   pragma Test_Statement;
   O.P5;
   pragma Test_Statement;
   O.P6;
   pragma Test_Statement;
end Test;
