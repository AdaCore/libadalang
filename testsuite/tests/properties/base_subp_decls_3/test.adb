procedure Test is
   package A is
      type T is tagged null record;

      procedure Foo (X : T) is null;
   end A;

   package B is
      type T is new A.T with null record;

      procedure Foo (X : T) is null;
   end B;

   package C is
      type T is new B.T with null record;

      procedure Foo (X : T) is null;
   end C;
begin
   null;
end Test;
