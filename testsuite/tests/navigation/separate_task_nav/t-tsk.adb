with Ada.Text_IO; use Ada.Text_IO;

separate (T)
task body Tsk is
begin
   accept B do
      Put_Line (A'Image);
      pragma Test_Statement;
   end B;
end Tsk;
