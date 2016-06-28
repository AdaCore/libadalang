procedure Lol is
   type Int is range 1 .. 110;
   type Int_2 is new Int;
   procedure A (I : Int_2);
begin
   A (12);
   pragma Test_Statement;
end Lol;
