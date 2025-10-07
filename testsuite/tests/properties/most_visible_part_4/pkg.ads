package Pkg is
   -- The most visible part of P, from the Pkg specification is P itself. But
   -- when origin lies in the package body, the most visible part is to be
   -- found there.
   procedure P;
   --% node.p_defining_name.p_most_visible_part(node)
   --% node.p_defining_name.p_most_visible_part(node.p_body_part())
end Pkg;
