procedure Test is
   type Kind is (A, C);

   procedure Process is null;

   X : Kind := A;
begin
   begin
      --  A and C should be resolved even though B's resolution fails
      case X is
         when A =>
            Process;
         when B =>
            Process;
         when C =>
            Process;
      end case;
   end;
   pragma Test_Block (Expect_Fail => True);
end Test;
