with Lists;

procedure Test is
   procedure Main is
      List : array (1 .. 2) of Integer;
      use Lists;
   begin
      for X of List (1 .. 2) loop
         null;
      end loop;
      pragma Test_Block;
   end Main;
begin
   Main;
end Test;
