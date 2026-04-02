package body Pkg7.Gen is
   type New_Integer is new Integer;
   function Identity (I : Integer) return Integer is
   begin
      return Integer (New_Integer (I));
   end Identity;
end Pkg7.Gen;
--% node.unit.root.p_is_preelaborable()
