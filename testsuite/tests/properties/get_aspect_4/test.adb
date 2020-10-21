procedure Test is
   type R is record
      X : Integer;
   end record;
   --% node.p_get_aspect("unchecked_union")
   pragma Unchecked_Union (R);
begin

end Test;
