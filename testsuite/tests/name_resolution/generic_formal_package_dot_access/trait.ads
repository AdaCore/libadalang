with Trait_Impl;

generic
package Trait is
   generic
      with package Impl is new Trait_Impl.Trait (<>);
   package Gen is
      procedure Foo (X : Impl.T);
   end Gen;
end Trait;
