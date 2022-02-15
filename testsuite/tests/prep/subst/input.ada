with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
   --  And $X will be...
   $X : constant String := "* this is $X *";
# if Y'Defined then
   $X should not be substitued here
# end if;
begin
   Put_Line ($x);
end Foo;
