package body Invalid is
   package body Nested is
      procedure Proc is separate;
      --% $node.p_syntactic_fully_qualified_name
   end Nested;

   procedure Sep_Subp is separate;
   --% $node.p_syntactic_fully_qualified_name
end Invalid;
