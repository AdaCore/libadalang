procedure Test is
   package Pkg is
      type T is tagged null record;

      procedure Foo (X : T) is null;
      procedure Bar (X : T) is null;
   end Pkg;

   package Der is
      type U is new Pkg.T with null record;

      overriding procedure Foo (X : U) is null;
      procedure Bar (X : U'Class) is null;

      procedure Test;
   end Der;

   package body Der is
      procedure Test is
         X : U;
      begin
         Foo (X);
         Bar (X);
      end Test;
   end Der;

   package Der_Der is
      type U is new Pkg.T with private;

      procedure Test;
   private
      type U is new Pkg.T with null record;

      overriding procedure Foo (X : U) is null;
   end Der_Der;

   package body Der_Der is
      procedure Test is
         X : U;
      begin
         Foo (X);
      end Test;
   end Der_Der;

   use Pkg;
   use Der;

   X : U;
begin
   Foo (X);
   Bar (X);
end Test;
