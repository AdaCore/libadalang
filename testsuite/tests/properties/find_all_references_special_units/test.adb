with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   package Int_IO is new Integer_IO (Integer);

   R : Integer;
   Last : Positive;
   Str : String := "42";
begin
   Int_IO.Get (Str, R, Last);
   pragma Find_All_References (Any, Previous_Referenced_Decl,
                               Show_Slocs => False);
end Main;
