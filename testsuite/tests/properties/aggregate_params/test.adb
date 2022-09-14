procedure Test is
   type Root is tagged record
      X : Integer := 1;
   end record;

   type Child is new Root with record
      Y : Integer := 2;
   end record;

   type Child_B is new Child with record
      Z : Integer := 3;
   end record;

   type Child_C is new Child_B with record
      T : Integer := 4;
   end record;

   C : Child := (Root with others => <>);
   --% node.f_default_expr.p_aggregate_params
   D : Child := (Root'(X => 9) with others => <>);
   --% node.f_default_expr.p_aggregate_params
   E : Child := (Root'(X => 9) with Y => 4);
   --% node.f_default_expr.p_aggregate_params
   F : Child := (others => <>);
   --% node.f_default_expr.p_aggregate_params

   G : Child_C := (Root with Y => <>, Z => <>, T => <>);
   --% node.f_default_expr.p_aggregate_params
begin
   null;
end Test;
