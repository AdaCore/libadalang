package body A is
   function Get_X return Integer is
      Y : Integer := X;
   begin
      return Y;
   end Get_X;

   function "+" (R : Rec_Type) return Integer is
   begin
      return R.U;
   end "+";
end A;
