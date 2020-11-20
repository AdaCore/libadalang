procedure Test is
   procedure Foo is null;
   pragma Find_All_References (Any, Follow_Renamings => True);

   procedure Bar renames Foo;

   procedure Baz renames Test.Bar;
begin
   Bar;
   Baz;
end Test;
