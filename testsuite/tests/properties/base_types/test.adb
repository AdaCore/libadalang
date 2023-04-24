--  Check that p_base_types on classwide types returns an empty set, and that
--  p_base_type is None.

procedure Test is
   type I is interface;

   type T is tagged null record;

   type U is new I and T with null record;
   --% node.p_base_types()
   --% node.p_classwide_type.p_base_types()
   --% node.p_classwide_type.p_base_type()
begin
   null;
end Test;
