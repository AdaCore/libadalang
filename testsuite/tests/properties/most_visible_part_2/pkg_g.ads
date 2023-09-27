generic
package Pkg_G is
   type T is private;

   X : constant T;

   procedure Foo (X : T);
private
   type T is null record;
   type U;

   X : constant T := (others => <>);
end Pkg_G;
