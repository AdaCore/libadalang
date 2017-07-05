separate (Foo)
package body Pkg is

   procedure Baz is
   begin
      pragma Test (I);
      I := I + 1;
   end Baz;

end Pkg;
