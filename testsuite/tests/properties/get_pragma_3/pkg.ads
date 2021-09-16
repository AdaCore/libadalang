package Pkg is
   procedure Foo;
   --% node.p_get_pragma("Inline")

   generic
   function GF_Function return Integer;
   --% node.p_get_pragma("inline")
end Pkg;
