with Ada.Text_IO; use Ada.Text_IO;

procedure Test_1 is
   package Pkg is
      generic
      package G is
         procedure Foo;
      end G;
   end Pkg;

   package body Pkg is
      package body G is
         procedure Foo is
         begin
            Put_Line ("HELLO");
         end Foo;
      end G;
   end Pkg;

   package Wow with Ghost is
      package I is new Pkg.G;
   end Wow;
begin
   Wow.I.Foo;
end Test_1;

