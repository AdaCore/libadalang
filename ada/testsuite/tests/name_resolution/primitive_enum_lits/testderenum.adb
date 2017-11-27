procedure Testderenum is
   type En is (A, B, C);

   type En2 is new En range B .. C;

   E : En2 := B;
begin
   null;
end Testderenum;
pragma Test_Block;
