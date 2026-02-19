package Defaults is
   function Foo (X : Integer) return Integer is (X)
      with Pre => (Static => X > 0, Runtime => True);
   pragma Test_Block;
end Defaults;
