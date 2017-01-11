with Ada.Text_IO;

procedure Put_Title (C : Character; S : String) is
begin
   Ada.Text_IO.Put_Line (S);
   Ada.Text_IO.Put_Line ((1 .. S'Length => C));
   Ada.Text_IO.New_Line;
end Put_Title;
