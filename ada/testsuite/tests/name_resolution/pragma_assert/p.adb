   procedure P is
      function "-"(I: Integer) return Integer is (I*I);

      I : constant Integer := -3;
   begin
      pragma Assert(I = 9);
      pragma Test_Statement;
   end P;
