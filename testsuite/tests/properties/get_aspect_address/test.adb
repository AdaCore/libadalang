procedure Test is
   A : Integer;
   --% node.p_get_aspect("address")
   --% node.p_get_at_clause()
   B : Integer := 2;

   for A use at B'Address;
begin
   null;
end Test;
