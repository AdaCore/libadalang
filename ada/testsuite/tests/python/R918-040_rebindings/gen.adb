package body Gen is
   package body Nested is
      function Foo return Integer is
      begin
         return 12;
      end Foo;

      function Bar return Poo is
      begin
         return (null record);
      end Bar;

      function Baz return Rec is (null record);
   end Nested;
end Gen;
