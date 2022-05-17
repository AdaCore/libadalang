procedure Test is
   type T is record
      X : Integer;
      --% node.p_get_pragma("atomic")
      pragma Atomic (X);
      --% node.p_associated_entities
   end record;
begin
   null;
end Test;
