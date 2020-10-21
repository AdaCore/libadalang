separate (B)
package body C is

   procedure Foo is
      A : Pouet := A2;
      pragma Test_Statement;
   begin
      null;
   end Foo;

   package body D is separate;
end C;
