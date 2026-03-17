procedure Test is
   package Pkg is
      type T is null record;

      procedure Foo (X : T) is null;
   end Pkg;

   package Other is
      subtype T is Pkg.T;

      procedure Foo (X : T) is null;
   end Other;

   use Other;
   use type Other.T;

   X : T;
begin
   Foo (X);
   --  not ambiguous: primitive defined in Pkg should not be made
   --  visible by the "use type" clause because it's not an operator.
end Test;
