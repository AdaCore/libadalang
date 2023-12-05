procedure Family is
   type Enume is (X, Y, Z);

   task T is
      entry E (1 .. 4);
      entry F (Enume);
   end T;

   task body T is
   begin
      null;
   end T;

   procedure P renames T.E (2);
   pragma Test_Statement;

   procedure P renames T.F (Z);
   pragma Test_Statement;
begin
   null;
end Family;
