procedure Test is
   package P is
      type T is tagged null record;

      function F (X : T) return T is (X);
      procedure G (X : T) is null;
   end P;

   package Q is
      type U is new P.T with null record;
   end Q;

   package R is
      type U is new Q.U with null record;

      overriding function F (X : U) return U is (X);
   end R;

   package I is
      type T is interface;

      procedure A (X : T) is null;

      type V is interface;

      procedure B (X : V) is null;

      type W is interface and T and V;
   end I;

   package S is
      type U is new R.U with null record;
   end S;

   package T is
      type U is new S.U and I.W with null record;
   end T;

   generic
   package G is
      type T is tagged null record;
      procedure A (X : T) is null;

      type I is interface;
      procedure B (X : I) is null;
      procedure C (X : I) is null;
   end G;

   package G_Test is
      package G_Inst is new G;

      type U is new G_Inst.T and G_Inst.I with null record;
      procedure B (X : U) is null;
   end G_Test;
begin
   null;
end Test;
