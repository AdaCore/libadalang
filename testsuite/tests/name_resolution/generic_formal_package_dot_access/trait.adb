package body Trait is
   package body Gen is
      procedure Foo (X : Impl.T) is
      begin
         Impl.Foo (X);
      end Foo;
   end Gen;
end Trait;
