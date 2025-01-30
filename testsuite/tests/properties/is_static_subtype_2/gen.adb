--  In a generic context, subtype of a generic formal object of mode in out,
--  and, result subtype of a generic formal function are never static.

procedure Gen is
   generic
      type T is range <>;
      with function F return Integer;
      --% node.f_subp_spec.f_subp_returns.p_is_static_subtype()
      A : in out  Natural;
      --% node.f_type_expr.p_is_static_subtype()
      B : Natural;
      --% node.f_type_expr.p_is_static_subtype()
   package G is
      subtype U is T range 1 .. 20;
      --% node.f_subtype.p_is_static_subtype()
      subtype V is U range 1 .. 10;
      --% node.f_subtype.p_is_static_subtype()
   end G;
begin
   null;
end Gen;
