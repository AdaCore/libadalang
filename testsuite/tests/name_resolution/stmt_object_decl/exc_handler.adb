procedure Exc_Handler is
   procedure Assert (B : Boolean) is null;
begin
   A : Integer := 15;
   begin
      A : Integer := 12;
      Assert (A = 12);
      pragma Test_Statement;

      raise Constraint_Error;
   exception
      when Constraint_Error =>
         Assert (A = 15);
         pragma Test_Statement;
    end;
end;
