package Foo is
   function "=" (I : Integer; S : String) return Boolean;
   --% node.p_corresponding_neq_subprogram

   function Func return Boolean;
   --% node.p_corresponding_neq_subprogram

   C : constant Integer := 1;
   --% node.p_corresponding_neq_subprogram
end Foo;
