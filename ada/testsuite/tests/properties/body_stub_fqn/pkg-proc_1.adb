separate (Pkg)
procedure Proc_1 is
   procedure Proc_2 is separate;
   --% $node.p_syntactic_fully_qualified_name
begin
   null;
end Proc_1;
