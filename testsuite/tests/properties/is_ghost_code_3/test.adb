procedure Test is
   function Foo (X : Integer) return Integer
      with Pre => True,
           Inline => True;
   --% node.f_aspects.f_aspect_assocs[0].p_is_ghost_code
   --% node.f_aspects.f_aspect_assocs[1].p_is_ghost_code

   function Foo (X : Integer) return Integer is (X);
begin
   null;
end Test;
