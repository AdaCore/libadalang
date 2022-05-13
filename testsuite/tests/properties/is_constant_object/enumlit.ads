package Enumlit is
   type E is (A);
   C : E renames A;
   --% node.p_is_constant_object
end Enumlit;
