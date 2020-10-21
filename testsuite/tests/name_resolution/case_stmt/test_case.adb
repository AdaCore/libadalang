procedure Test_Case is
   function Foo return Integer is (12);
   function Foo return Float is (12.0);
   procedure Foo is null;
begin
   begin
      case Foo is
         when 12 => null;
         when others => null;
      end case;
   end;
   pragma Test_Block;
end Test_Case;
