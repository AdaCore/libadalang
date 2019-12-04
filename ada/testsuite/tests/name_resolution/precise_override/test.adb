procedure Test is
   package P is
      type T is tagged null record;
      procedure Foo (X : T) is null;
   end P;

   package Q is
      type U is new P.T with null record;
      overriding procedure Foo (X : U) is null;
   end Q;

   X : Q.U := Q.U'(null record);
begin
   X.Foo;
   pragma Test_Statement;
end Test;
