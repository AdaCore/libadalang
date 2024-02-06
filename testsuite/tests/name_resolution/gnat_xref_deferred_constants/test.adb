procedure Test is
   package P is
      U, V : constant Integer;
   private
      U, V : constant Integer := 1;
   end P;
begin
   null;
end Test;
