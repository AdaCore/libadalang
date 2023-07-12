procedure Test is
   type A is new Integer;
   --% node.p_is_scalar_type()
   type B is range 1 .. 12;
   --% node.p_is_scalar_type()
   type C is new Float;
   --% node.p_is_scalar_type()
   type D is null record;
   --% node.p_is_scalar_type()
   type E is new D;
   --% node.p_is_scalar_type()
   subtype F is E;
   --% node.p_is_scalar_type()
begin
   null;
end Test;
