package body A is
   function Get_X return Integer is
      Y : Integer := X;
      pragma Find_All_References (Any);
   begin
      return Y;
   end Get_X;
   pragma Find_All_References (Any);

   function "+" (R : Rec_Type) return Integer is
   begin
      return R.U;
   end "+";
end A;
