procedure Arr is

   type Int_Rec is record
      I : Integer;
   end record;

   type Vector is array(Integer  range <>) of Float;

   I : Integer := 1;
   R : Int_Rec := (I => 4);
   Vec : Vector := (1.0, 2.0, 3.0);
begin
   Vec := (Vec with delta I => 3.0);
   pragma Test_Statement;

   Vec := (Vec with delta R.I => 3.0);
   pragma Test_Statement;
end Arr;
