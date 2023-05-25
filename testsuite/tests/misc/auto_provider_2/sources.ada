separate (Foo)
procedure Bar (Value : Enum) is
begin
   Do_Things;
   pragma Test_Statement;
end Bar;

package body Foo is
   procedure Do_Things is null;
   procedure Bar (Value : Enum) is separate;
   pragma Test_Statement;
end Foo;

package Foo is
   type Enum is (A, B, C);
   procedure Bar (Value : Enum);
   pragma Test_Statement;
end Foo;
