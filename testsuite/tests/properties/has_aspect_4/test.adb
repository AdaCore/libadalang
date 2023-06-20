procedure Test is

   type T is record
      A : Integer;
         --% node.p_has_aspect ("Independent")
      pragma Independent (A);
      B : Integer;
      --% node.p_has_aspect ("Independent")
      C : Integer;
      --% node.p_has_aspect ("Independent")
      pragma Independent (B);
      D : Integer with Independent;
      --% node.p_has_aspect ("Independent")
   end record;
   --% node.p_has_aspect ("Independent")

begin
   null;
end Test;
