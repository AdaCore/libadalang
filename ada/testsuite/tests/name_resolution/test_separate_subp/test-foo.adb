separate (Test)
procedure Foo is
   procedure Bar is
   begin
      J := 15;
   end Bar;
begin
   null;
end Foo;
pragma Test_Block;
