procedure Test is
   type T is (A, B, C);

   X : T;
begin
   begin
      case X is
         when T range A .. B =>
            null;
         when others =>
            null;
      end case;
   end;
   pragma Test_Block;
end Test;
