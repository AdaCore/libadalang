with Ada.Containers.Indefinite_Ordered_Sets;

procedure Test is
   package My_Indefinite_Ordered_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => String);
   My_Set_1 : My_Indefinite_Ordered_Sets.Set;
   My_Cursor_1 : My_Indefinite_Ordered_Sets.Cursor;
begin
   for C in My_Set_1.Iterate (My_Cursor_1) loop
      null;
   end loop;
   pragma Test_Block;
end Test;
