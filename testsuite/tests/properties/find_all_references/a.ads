package A is
   pragma Find_All_References (Any, "A");

   type Rec_Type is record
      U : Integer;
      pragma Find_All_References (Any);
      V : Integer;
   end record;
   pragma Find_All_References (Any);

   function Get_X return Integer;

   function "+" (R : Rec_Type) return Integer;
   pragma Find_All_References (Any);

private
   X, Dummy : Integer;
   pragma Find_All_References (Any);
end A;
