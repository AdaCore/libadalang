with Types; use all type Types.Enum;

procedure Test is
   X : Types.Enum;
begin
   begin
      case X is
         when A => null;
         when B => null;
         when others => null;
      end case;
   end;
   pragma Test_Block;
end Test;
