procedure Main is
   procedure F (X : access procedure (Y : Integer));

   procedure F (X : access procedure (Y : Integer)) is
   begin
      null;
   end F;
   --% $node.p_canonical_part()

   procedure G (X : access function (Y : Integer) return Integer);

   procedure G (X : access function (Y : Integer) return Integer) is
   begin
      null;
   end G;
   --% $node.p_canonical_part()
begin
   null;
end Main;
