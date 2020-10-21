with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   generic
      A : Integer;
   procedure Foo;

   J : Integer;
   procedure Foo is separate;
begin
   null;
end Test;
