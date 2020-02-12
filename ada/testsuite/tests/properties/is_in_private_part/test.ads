package Test is
   function Foo return Integer;
   --% $node.p_is_in_private_part

private

   function Bar return Integer;
   --% $node.p_is_in_private_part
end Test;
