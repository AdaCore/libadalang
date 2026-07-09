with Types; use Types;

--  'Base'Last of a generic formal type is not static, so the relation is not
--  a static expression.
generic
   type Index_Type is range <>;
package Pkg is
   B : constant Boolean :=
     Index_Type'Base'Last >= Count_Type'Pos (Count_Type'Last);
   --% node.f_default_expr.p_is_static_expr()
end Pkg;
