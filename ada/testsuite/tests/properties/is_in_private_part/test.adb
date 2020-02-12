package body Test is
   function Baz return Integer is (42);
   --% $node.p_is_in_private_part
end Test;
