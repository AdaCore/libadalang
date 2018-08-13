with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   package Foo is
      I : Integer := 0;
      procedure Bar;
   end Foo;

   J : Integer := 0;

   package body Foo is separate;
begin
   null;
end Test;
