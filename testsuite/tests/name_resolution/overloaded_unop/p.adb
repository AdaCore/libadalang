   procedure P is
      function "-"(I: Integer) return Integer is (I*I);

      I : constant Integer := -3;
      pragma Test_Statement;
   begin
      Pragma Assert(I = 9);
   end P;
