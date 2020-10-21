procedure Test is
   type T is (A, B, C)
      with Convention => C;
   pragma Test_Block;

   pragma Convention (C, T);
   pragma Test_Statement;
begin
   null;
end Test;
