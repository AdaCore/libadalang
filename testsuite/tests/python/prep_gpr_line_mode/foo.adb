with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
begin
# if Foo = "bar" then
   Put_Line ("Hello, world!");
# else
   null;
# end if;
end Foo;
