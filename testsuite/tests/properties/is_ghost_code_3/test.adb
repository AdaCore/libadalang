with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

procedure Test is
   function Foo (X : Integer) return Integer
      with Pre => True,
           Inline => True;
   --% node.f_aspects.f_aspect_assocs[0].p_is_ghost_code
   --% node.f_aspects.f_aspect_assocs[1].p_is_ghost_code

   function Foo (X : Integer) return Integer is (X);

begin
   Integer'Write (Stream_Access'(null), 12);
   --  Since there is no reference for Integer'Write, this tests checks
   --  that calling `is_ghost_code` does not crash on null references.
   --% node.p_is_ghost_code

   pragma Assert_And_Cut (True);
   --% node.p_is_ghost_code
end Test;
