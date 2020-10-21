procedure Testloop is
begin
   A : loop
      B : loop
         <<C>>
         goto C;
         pragma Test_Statement;

         goto A.B.C;
         pragma Test_Statement;
      end loop B;
   end loop A;
end Testloop;
