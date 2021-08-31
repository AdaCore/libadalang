package body Test is

   function F(A : R) return R is
     (A);

   procedure P(A : R) is
   begin
      Put_Line("P of R");
   end P;

   function F(A : S) return S is
     (A);

   procedure P(A : S) is
   begin
      Put_Line("P of S");
   end P;

   procedure Test is
      O1 : S;
      O2 : R'Class := O1;
   begin
      O2
        .
        F
        --% node.p_is_dispatching_call()
        .
        P
        --% node.p_is_dispatching_call()
        ;
   end Test;

end Test;
