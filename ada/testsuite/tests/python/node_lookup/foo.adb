limited with Ada.Text_IO;

procedure Foo is
   function "+" (S : String) return String is (S);
begin
   Ada.Text_IO.Put_Line (+"Hello, world!");
end Foo;
