with Ada.Text_IO;

procedure Foo is
   function Fact (N : Integer) return Integer is
   begin
      if N > 0 then
         return N * Fact (N - 1);
      else
         return 1;
      end if;
   end Fact;
begin
   Put_Line (Integer'Image (Fact (6)));
end Foo;
