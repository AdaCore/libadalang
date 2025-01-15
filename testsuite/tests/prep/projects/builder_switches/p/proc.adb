with Ada.Text_IO; use Ada.Text_IO;

procedure Proc is
begin

#if X'Defined then
   Put_Line ("X");
#elsif Y'Defined then
   Put_Line ("Y");
#else
   invalid code
#end if;

#if X2'Defined then
   Put_Line ("X2");
#elsif Y2'Defined then
   Put_Line ("Y2");
#else
   invalid code
#end if;

end Proc;
