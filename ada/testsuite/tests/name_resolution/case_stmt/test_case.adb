with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Case is
   function Foo return Integer is (12);
   function Foo return Float is (12.0);
   procedure Foo is null;
begin
   case Foo is
      when 12 => Put_Line ("I KNEW IT");
      when others => null;
   end case;
   pragma Test_Statement;
end Test_Case;
