procedure Test (S : Integer) is
begin
   declare
      S : constant String := "Other_S";
      T : constant String := Integer'Image (S);
      pragma Test_Statement (Expect_Fail => True);
   begin
      null;
   end;
end Test;
