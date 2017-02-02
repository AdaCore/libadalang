package body Foo is
   package body Nested is
      procedure Bar is
      begin
         Put_Line ("Hello, world!");
      end Bar;
   end Nested;
end Foo;
