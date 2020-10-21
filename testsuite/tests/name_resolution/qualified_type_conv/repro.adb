procedure Repro is
   package Pouet is
      type T is new Integer;
   end Pouet;
begin
   begin
      case Pouet.T (12) is
         when 12 => null;
         when others => null;
      end case;
   end;
   pragma Test_Block;

   declare
      use Pouet;
   begin
      case T (12) is
         when 12 => null;
         when others => null;
      end case;
   end;
   pragma Test_Block;
end Repro;
