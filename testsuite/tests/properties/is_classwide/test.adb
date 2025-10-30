procedure Test is
   type A is tagged null record;
   --% node.p_is_classwide

   subtype B is A'Class;
   --% node.p_is_classwide

   subtype C is B;
   --% node.p_is_classwide

   procedure D (Arg : A'Class);
   --% node.f_subp_spec.p_params[0].p_formal_type().p_is_classwide

   type E is range 1 .. 2;
   --% node.p_is_classwide
begin
   null;
end Test;
