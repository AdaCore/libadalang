package Test is
   X : constant Integer;
private
   X : constant Integer := 1;
   --% dn = node.p_defining_names[0]
   --% cp = dn.p_canonical_part()
   --% cp.p_most_visible_part(cp)
   --% cp.p_most_visible_part(dn)
end Test;
