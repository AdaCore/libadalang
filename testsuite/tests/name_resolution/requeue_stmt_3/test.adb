procedure Test is
   protected type PT is
      entry Process;
   end PT;

   protected body PT is
      entry Process when True is
      begin
         null;
      end Process;
   end PT;

   type My_Range is range 1 .. 2;
   type My_Array is array (My_Range) of PT;
   Arr : My_Array;

   function F (I, J : Float) return My_Range is
   begin
      return My_Range (I * J);
   end F;

   task T is
      entry P (Name : String);
   end T;

   task body T is
      Index : My_Range := 1;
   begin
      accept P (Name : String) do
         requeue Arr (Index).Process;
         pragma Test_Statement;

         requeue Arr (F (4.3, 8.9)).Process;
         pragma Test_Statement;
      end P;
   end T;
begin
   null;
end Test;
