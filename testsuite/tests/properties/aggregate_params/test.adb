procedure Test is
   type Root is tagged record
      X : Integer := 1;
   end record;

   type Child is new Root with record
      Y : Integer := 2;
   end record;

   C : Child := (Root with others => <>);
   --% node.f_default_expr.p_aggregate_params
   D : Child := (Root'(X => 9) with others => <>);
   --% node.f_default_expr.p_aggregate_params
   E : Child := (Root'(X => 9) with Y => 4);
   --% node.f_default_expr.p_aggregate_params
   F : Child := (others => <>);
   --% node.f_default_expr.p_aggregate_params
begin
   null;
end Test;
