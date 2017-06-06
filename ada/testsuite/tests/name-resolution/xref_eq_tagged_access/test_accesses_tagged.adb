procedure Test is
   type Integer is range 0 .. 100;

   type Rec is tagged record
      B, C : Integer;
   end record;

   type Rec_Child is new Rec with record
      D, E : Integer;
   end record;

   type Rec_Child_Child is new Rec_Child with null record;


   type Rec_Access is access all Rec;
   type Rec_Child_Access is access all Rec_Child;

   I : Rec_Child_Access;

   J : Rec_Child;
begin
   J.D := 12;
   pragma Test_Statement;

   J.B := 12;
   pragma Test_Statement;

   I.all.D := 14;
   pragma Test_Statement;

   I.all.B := 14;
   pragma Test_Statement;

   I.D := 15;
   pragma Test_Statement;

   I.B := 15;
   pragma Test_Statement;
end Test;
