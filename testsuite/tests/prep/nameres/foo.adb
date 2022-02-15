with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
#if X'Defined then
   procedure $X is
   begin
      Put_Line ("$X");
   end $X;
#else
   procedure Bar is null;
#end if;
begin
   Bar;
   pragma Test_Statement;
end Foo;
