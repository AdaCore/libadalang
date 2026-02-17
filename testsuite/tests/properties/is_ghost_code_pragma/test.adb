procedure Test is
   X : Integer;
   --% node.p_is_ghost_code
   Y : Integer;
   --% node.p_is_ghost_code
   pragma Ghost;

   Z : Integer := X;
   --% node.p_is_ghost_code
   pragma Ghost (False);
begin
   null;
end Test;
