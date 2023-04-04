procedure Test is
   generic
      type T is private;
   function GF (X : T) return Integer;

   function GF (X : T) return Integer is
   begin
      return 1;
   end GF;

   package PT is
      type T is range 1 .. 99;

      function F is new GF (T);
   end PT;

   package Q is
      type T is new PT.T;
   end Q;

   package P is
      function G (A : Q.T) return Integer;
   end P;

   package body  P is
      function G (A : Q.T) return Integer is
      begin
         return Q.F (A);
         pragma Test_Statement;
      end G;
   end P;
begin
   null;
end Test;
