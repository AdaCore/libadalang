procedure Test is
   package P is
      type Root is tagged null record;

      procedure P2 (R : Root) is null;
      --% root = node.p_root_subp_declarations()[0]
      --% root_type = root.p_subp_spec_or_null().p_param_types()[0]

      type Child is new Root with null record;

      overriding procedure P2 (C : Child) is null;
      --% child = node.p_root_subp_declarations()[0]
      --% child_type = child.p_subp_spec_or_null().p_param_types()[0]

      --% (root == child)
      --% (root_type == child_type)
   end P;
begin
   null;
end Test;
