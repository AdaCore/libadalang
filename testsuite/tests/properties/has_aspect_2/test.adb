procedure Test is
   A : constant Boolean := True;
   B : constant Boolean := False;

   procedure Pouet0 is null;
   --% $node.p_has_aspect('inline')

   procedure Pouet1 is null
      with Inline;
   --% $node.p_has_aspect('inline')

   procedure Pouet2
      with Inline;

   procedure Pouet2 is null;
   --% $node.p_has_aspect('inline')

   package Pkg is
      procedure Pouet3;
      --% $node.p_has_aspect('inline')
   end Pkg;

   package body Pkg is
      procedure Pouet3 is null;

      pragma Inline (Pouet3);
   end Pkg;

   generic
   procedure Pouet3 with Inline;
   --% node.p_has_aspect('inline')

   procedure Pouet3 is null;
   --% node.p_has_aspect('inline')
begin
   null;
end Test;
