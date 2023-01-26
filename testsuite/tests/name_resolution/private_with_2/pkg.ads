private with Foo;

package Pkg is
   procedure Test;

   package Nested is
      procedure Test;
   private
      X : Foo.T;
   end Nested;
end Pkg;
