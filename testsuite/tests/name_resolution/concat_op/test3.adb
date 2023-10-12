with Ada.Text_IO;

procedure Main is

   function "+" (Left, Right : String) return String is
   begin
      return Left & Right;
   end "+";

   Arg1 : constant String := "arg1";
   Arg2 : constant String := "arg2";
begin

   Ada.Text_IO.Put_Line (Arg1 + Arg2 & "some string");
   pragma Test_Statement;

   Ada.Text_IO.Put_Line ((Arg1 + Arg2) & "some string");
   pragma Test_Statement;

end Main;
