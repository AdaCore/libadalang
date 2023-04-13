package body Q.S is
   function F (A : P.T) return Integer is
      I : Integer := P.First (A).Val;
      pragma Test_Statement;
   begin
      return I;
   end F;
end Q.S;
