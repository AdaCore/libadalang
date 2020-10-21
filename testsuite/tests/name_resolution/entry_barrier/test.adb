procedure Test is
   protected type PT is
      entry E;
   end PT;

   function F return Integer is (42);

   protected body PT is
      entry E when F = 42 is
      begin
         null;
      end E;
      pragma Test_Statement;
   end PT;
begin
   null;
end Test;
