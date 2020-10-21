procedure Test_Result_Attr is
   function Barize return Integer
      with Post => Barize'Result = 12;

   function Barize return Integer is
   begin
      return 12;
   end Barize;
begin
end Test_Result_Attr;
pragma Test_Block;
