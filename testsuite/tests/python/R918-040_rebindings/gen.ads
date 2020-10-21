generic
package Gen is
   type Rec is null record;
   generic
   package Nested is
      type Poo is null record;
      function Foo return Integer;
      function Bar return Poo;
      function Baz return Rec;
   end Nested;
end Gen;
