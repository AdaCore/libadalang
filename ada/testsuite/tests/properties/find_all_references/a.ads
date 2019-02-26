package A is

   type Rec_Type is record
      U : Integer;
      V : Integer;
   end record;

   function Get_X return Integer;

   function "+" (R : Rec_Type) return Integer;

private
   X : Integer;
end A;
