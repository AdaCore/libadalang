private with Foo;
pragma Elaborate (Foo);
pragma Test_Statement;

package Pkg is
   procedure Test;

   package Nested is
      procedure Test;
   private
      X : Foo.T;
   end Nested;
end Pkg;
