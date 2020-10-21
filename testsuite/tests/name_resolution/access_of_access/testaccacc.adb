procedure Testaccacc is
   type A is access all Integer;
   type AA is access all A;

   Inst : AA := new A'(new Integer'(12));

   O : Integer;
begin
   O := Inst.all.all;
   pragma Test_Statement;
end Testaccacc;
