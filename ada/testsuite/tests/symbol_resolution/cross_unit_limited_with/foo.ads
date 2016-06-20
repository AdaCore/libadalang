limited with Bar;

package Foo is

   type Foo_Type is record
      B : access Bar_Type;
   end record;

   F : constant Foo_Type := (B => null);

   pragma Test (F.B);
end Foo;
