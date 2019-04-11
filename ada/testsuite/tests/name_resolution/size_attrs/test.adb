with Ada.Text_IO;
use Ada.Text_IO;

procedure Main is
   type R is record
      X : Integer;
   end record;

   X : R;
begin
   Put_Line (Integer'Image (R'Size));
   pragma Test_Statement;

   Put_Line (Integer'Image (X'Size));
   pragma Test_Statement;

   Put_Line (Integer'Image (R'Object_Size));
   pragma Test_Statement;

   Put_Line (Integer'Image (R'Value_Size));
   pragma Test_Statement;

   Put_Line (Integer'Image (R'Max_Size_In_Storage_Elements));
   pragma Test_Statement;
end Main;
