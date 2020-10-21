procedure Bar is
   package Bar is
      procedure Bar;
      --% $node.p_body_part()
   end Bar;
   --% $node.p_body_part

   package body Bar is
      procedure Bar is
      begin
         null;
      end Bar;
      --% $node.p_decl_part()
   end Bar;
   --% $node.p_decl_part()
begin
   null;
end Bar;
--% $node.p_decl_part()
