package A is

   type Rec_Type is record
      U : Integer;
      V : Integer;
   end record;

   function Get_X return Integer;

private
   X : Integer;
end A;