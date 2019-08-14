procedure Test is
   A : constant Boolean := True;
   B : constant Boolean := False;

   procedure Pouet0 is null;
   --% $node.p_has_aspect('inline')

   procedure Pouet1 is null
      with Inline;
   --% $node.p_has_aspect('inline')

   procedure Pouet2 is null
      with Inline => A and B;
   --% $node.p_has_aspect('inline')

   procedure Pouet3 is null
      with Inline => A or B;
   --% $node.p_has_aspect('inline')

   procedure Pouet4 is null
      with Inline => True;
   --% $node.p_has_aspect('inline')

   procedure Pouet5 is null
      with Inline => False;
   --% $node.p_has_aspect('inline')
begin
   null;
end Test;
