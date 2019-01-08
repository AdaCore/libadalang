procedure Repro is
   package Pouet is
      type T is new Integer;
   end Pouet;
begin
   case Pouet.T (12) is
      when 12 => null;
      when others => null;
   end case;
   pragma Test_Statement;

   declare
      use Pouet;
   begin
      case T (12) is
         when 12 => null;
         when others => null;
      end case;
      pragma Test_Statement;
   end;
end Repro;
