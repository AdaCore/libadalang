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

   type Small is record
      A, B : Character;
   end record;
   --% $node.p_has_aspect('size')
   for Small'Size use 0;

   type Angle is delta Pi/2.0**31 range -Pi .. Pi;
   --% $node.p_has_aspect('small')
   for Angle'Small use 0.001;

   function Foo (X : Integer) return Boolean is (True);
   function Bar (X : Integer) return Integer is (42)
      with Pre => Foo (X),
           Pre'Class => Foo (X),
           Invariant => Foo (X);
   --% node.p_has_aspect("pre")
   --% node.p_has_aspect("pre'class")
   --% node.p_has_aspect("invariant")
begin
   null;
end Test;
