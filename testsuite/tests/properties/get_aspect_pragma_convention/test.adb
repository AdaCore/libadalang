procedure Test is
   X : Integer := 0
      with Convention => C;
   --% node.p_get_aspect("Convention").value

   Y : Integer := 0;
   --% node.p_get_aspect("Convention").value
   pragma Convention (C, Y);
begin
   null;
end Test;

