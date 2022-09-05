with Pkg;     use Pkg;
with Pkg.Der; use Pkg.Der;

procedure Test is
   X : U'Class := U'(null record);
begin
   Foo (X);
   --  This used to resolve to `Foo` from pkg.ads because it was "used" first,
   --  but it should actually resolve to the declaration in pkg-der.ads
   --  because it's more accurate.
   pragma Test_Statement;
   X.Foo;
   pragma Test_Statement;
end Test;
