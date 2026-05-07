procedure Test is
   type A is new Integer;
   --% node.p_is_scalar_type()
   type B is range 1 .. 12;
   --% node.p_is_scalar_type()
   type C is new Float;
   --% node.p_is_scalar_type()
   type D is (F_1, F_2);
   --% node.p_is_scalar_type()
   type E is null record;
   --% node.p_is_scalar_type()
   type F is new E;
   --% node.p_is_scalar_type()
   subtype G is F;
   --% node.p_is_scalar_type()
begin
   null;
end Test;
