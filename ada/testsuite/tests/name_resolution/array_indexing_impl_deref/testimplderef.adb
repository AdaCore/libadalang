procedure Testimplderef is
   type Rec is record
      B : Integer;
      C : Integer;
   end record;
   type Ptr_Rec is access all Rec;
   type Ptr_Ptr_Rec is access all Ptr_Rec;


   type Arr is array (1 .. 10) of Natural;
   type Ptr_Arr is access all Arr;
   type Ptr_Ptr_Arr is access all Ptr_Arr;

   Inst_1 : Ptr_Ptr_Arr := new Ptr_Arr'(new Arr'(others => 12));
   Inst_2 : Ptr_Arr := new Arr'(others => 12);

   Inst_3 : Ptr_Ptr_Rec := new Ptr_Rec'(new Rec'(others => 12));
   Inst_4 : Ptr_Rec := new Rec'(others => 12);
begin

   Inst_1 (2) := 12;

   pragma Test (Inst_2 (2));

   Inst_2 (2) := 12;
   pragma Test_Statement;

   Inst_3.B := 12;
   Inst_4.B := 12;

end Testimplderef;
