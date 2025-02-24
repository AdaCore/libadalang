procedure Test is
   -- Check that subtype Predicate aspects are correctly inherited from parent
   -- subtypes (most of aspects are not allowed on subtype declarations but a
   -- few are, in that case, we must recurse on parent subtypes instead of base
   -- type when looking for them).

   subtype S1 is Integer;
   subtype S2 is S1 with Static_Predicate => true;
   subtype S3 is S2;
   --% node.p_get_aspect("Predicate")
   --% node.p_get_aspect("Static_Predicate")

   pragma Predicate (S2, true);
begin
   null;
end;
