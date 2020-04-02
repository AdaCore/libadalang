procedure Test is
   type Rec (X : Integer) is record
      Y : Integer;
   end record;

   type Rec_Ptr is access Rec;

   subtype Rec_Ptr_Constrained is Rec_Ptr (X => 12);
   pragma Test_Statement;

   X : Rec_Ptr (X => 12);
   pragma Test_Statement;
begin
   null;
end Test;
