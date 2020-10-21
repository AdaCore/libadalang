procedure Test_Access is
   type Int is range 0 .. 1000000000;

   type Rec is record
      A, B : Int;
   end record;

   type Rec_Access is access all Rec;

   R   : Rec := (A => 12, B => 15);
   R_A : Rec_Access;
begin
   R_A.all := R;
   pragma Test_Statement;
end Test_Access;
