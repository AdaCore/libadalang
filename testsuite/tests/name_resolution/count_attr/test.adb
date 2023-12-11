-- This test ensures that 'Count attribute can resolve when used on an entry
-- with a family memer.
procedure Test is
   type Enume is (X, Y, Z);

   task T is
      entry E (1 .. 4);
      entry F (Enume);
      entry G (Enume) (A, B : Float);
   end T;

   task body T is
      I : Integer := E (3)'Count;
      pragma Test_Statement;

      J : Integer := F (X)'Count;
      pragma Test_Statement;

      K : Integer := G (Y)'Count;
      pragma Test_Statement;
   begin
      null;
   end T;

   protected type PT is
      entry E (X : Float);
   end PT;

   protected body PT is
      function C return Integer is
      begin
         return E'Count;
         pragma Test_Statement;
         -- This is just a non-regression test
      end C;

      entry E (X : Float) when True is
      begin
         null;
      end E;
   end PT;
begin
   null;
end Test;
