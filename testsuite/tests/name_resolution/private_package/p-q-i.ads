package P.Q.I is
   Y : P.T;
   pragma Test_Statement;

   Z : Q.T;
   pragma Test_Statement(Expect_Fail => True);

   package K is
      Inner_Public_Y : P.T;
      pragma Test_Statement;

      Inner_Public_Z : Q.T;
      pragma Test_Statement(Expect_Fail => True);
   private
      Inner_Private_Y : P.T;
      pragma Test_Statement;

      Inner_Private_Z : Q.T;
      pragma Test_Statement(Expect_Fail => True);
   end K;
private
   package L is
      Private_Y : P.T;
      pragma Test_Statement;

      Private_Z : Q.T;
      pragma Test_Statement;
   end L;
end P.Q.I;
