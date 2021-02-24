procedure Test is
   procedure New_Line is null;

   procedure New_Line (B : Boolean) is
   begin
      New_Line;
      pragma Test_Statement;

      if Test.New_Line.B then
         null;
      end if;
      pragma Test_Statement;
   end New_Line;
begin
   null;
end Test;
