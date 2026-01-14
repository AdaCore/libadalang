with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure P1 (Low, High : Integer) is
      subtype R1 is Integer range 1 .. 10;
      subtype R2 is Integer range Low .. High;
   begin
      Put_Line (R1'Range_Length'Image);
      Put_Line (R2'Range_Length'Image);
   end;

   type Enum is (A, B, C);
   subtype Enum_Sub_Range is Enum range A .. B;
   subtype Enum_Null_Range is Enum range B .. A;
   subtype My_Char is Character range 'A' .. 'z';
begin
   P1(0, 10);
   Put_Line (Integer'Range_Length'Image);
   Put_Line (Boolean'Range_Length'Image);
   Put_Line (Enum'Range_Length'Image);
   Put_Line (Enum_Sub_Range'Range_Length'Image);
   Put_Line (Enum_Null_Range'Range_Length'Image);
   Put_Line (Character'Range_Length'Image);
   Put_Line (Wide_Character'Range_Length'Image);
   Put_Line (Wide_Wide_Character'Range_Length'Image);
   Put_Line (My_Char'Range_Length'Image);
end;
