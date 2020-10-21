separate (Test)
package body Foo is
   procedure Bar is
   begin
      I := 12;
      J := 15;
   end Bar;
end Foo;
pragma Test_Block;
