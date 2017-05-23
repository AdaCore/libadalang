package Foo is
   generic
      type Array_Type is array (Positive range <>) of Integer;
   procedure Reset (A : out Array_Type);
end Foo;
