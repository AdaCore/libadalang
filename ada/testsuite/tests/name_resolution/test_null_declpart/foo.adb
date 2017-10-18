package body Foo is
   procedure P (Value : T);

   procedure P (Value : T) is null;
   procedure Dummy is null;
end Foo;
