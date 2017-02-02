package Foo is
   generic
      with procedure Put_Line (S : String);
   package Nested is
      procedure Bar;
   end Nested;
end Foo;
