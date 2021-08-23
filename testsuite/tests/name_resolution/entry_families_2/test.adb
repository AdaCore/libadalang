procedure Test is
   type Enum1 is (A, B);
   type Enum2 is (A, B);

   task T is
      entry E (1 .. 1);
      entry F (Enum2);
   end T;

   task body T is
   begin
      accept E (1);
      pragma Test_Statement;

      accept F (A);
      pragma Test_Statement;
   end T;
begin
   T.E (1);
   pragma Test_Statement;

   T.F (B);
   pragma Test_Statement;
end Test;
