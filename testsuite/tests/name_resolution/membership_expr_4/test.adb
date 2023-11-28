procedure Test is
   type T is (A, B, C, D);
   type U is new T;
   type U_Ptr is access U;

   function F (O : access T) return Boolean is
   begin
      return O in U_Ptr;
      pragma Test_Statement;
   end F;
begin
   null;
end Test;
