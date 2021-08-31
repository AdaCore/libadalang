package Test is

   type R is tagged
     null record;

   function F(A : R) return R;

   procedure P(A : R);

   type S is new R with
     null record;

   overriding function F(A : S) return S;

   overriding procedure P(A : S);

   procedure Test;

end Test;
