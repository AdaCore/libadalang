package Test is
   package Int is
      function Bar (X : Integer) return Integer is (X);
   end Int;

   function Foo1 (Int : Integer) return Integer renames Int.Bar;
   pragma Test_Statement;
end Test;
