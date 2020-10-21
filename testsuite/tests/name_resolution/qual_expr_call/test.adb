procedure Test is
   type Rec is record
      X : Integer;
   end record;

   function Make_Rec (X : Integer) return Rec is
   begin
      return (X => X);
   end Make_Rec;

   type Rec_Func is access function (X : Integer) return Rec;

   F : Rec_Func := Make_Rec'Access;

   R : Rec;
   X : Integer;
begin
   X := Rec_Func'(F) (2).X;
   pragma Test_Statement;
end Test;
