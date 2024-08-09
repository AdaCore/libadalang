with Trait;
with Trait_Impl;

generic
   with package Impl is new Trait (<>);
package Pkg is
   generic
      type T is private;
   package Gen is
      procedure Foo (X : T) is null;
      package My_Trait_Impl is new Trait_Impl.Trait (T, Foo);
      package My_Impl is new Impl.Gen (My_Trait_Impl);
      pragma Test_Statement;
   end Gen;
end Pkg;
