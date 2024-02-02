procedure Test is
   function F
     (I : in     Integer;
      J : in out Integer;
      K : out    Integer;
      L : access Integer) return Integer is
   begin
      return 0;
   end F;
   --  I, J, K, L references to F should be ignored by gnat_compare
begin
   null;
end;
