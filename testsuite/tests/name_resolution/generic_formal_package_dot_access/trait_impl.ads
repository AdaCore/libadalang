package Trait_Impl is
   generic
      type T is private;
      with procedure Foo (X : T);
   package Trait is
   end Trait;
end Trait_Impl;
