package Pkg2 is
   procedure Foo;
   --% $node.p_body_part

   type T is private;
   --% $node.p_full_view

private

   type T;
end Pkg2;
--% $node.p_body_part
