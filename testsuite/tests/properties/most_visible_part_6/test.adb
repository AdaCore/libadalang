package body Test is
   function Constructor (Param : Integer) return Boolean is
   begin
      return Param > 42;
   end Constructor;
   --% p = node.f_subp_spec.p_params[0].p_defining_names[0]
   --% p.p_most_visible_part(p)
end Test;
