with Ada.Text_IO; use Ada.Text_IO;

procedure Proc2 is
begin

#if X3'Defined then
   Put_Line ("X3");
#elsif Y3'Defined then
   Put_Line ("Y3");
#else
   invalid code
#end if;

end Proc2;
