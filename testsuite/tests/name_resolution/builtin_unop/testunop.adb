procedure TestUnop is
   package P is
      package E is
         type T is range 1 .. 100;
      end E;
   end P;

   A, B : P.E.T;
begin
   B := 5;
   pragma Test_Statement;

   A := P.E."+" (B);
   pragma Test_Statement;
end TestUnop;
