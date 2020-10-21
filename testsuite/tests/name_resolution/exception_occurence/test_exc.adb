with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Exc is begin
   raise Constraint_Error with "Lol what a complicated way to print";
exception
when E : Constraint_Error =>
   Put_Line (Exception_Message (E));
   pragma Test_Statement_UID;
end Test_Exc;
