package body Aaa.Aaa is
   procedure P is
      B : Integer;
   begin
      B := Aaa.B;
      B := Standard.Aaa.B;
      B := Standard.Aaa.Aaa.B;
   end P;
end Aaa.Aaa;
pragma Test_Block;
