procedure Testnull is
   type Int_Ptr is access Integer;

   A : Integer := 12;
   B : Int_Ptr := A'Access;
   pragma Test_Statement;
begin
   A := null;
   B := null;
end Testnull;
