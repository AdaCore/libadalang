procedure Test is
   protected type PT is
      entry E;
      --% node.p_body_part()

      entry E (X : Integer);
      --% node.p_body_part()

      entry F (1 .. 5) (X : Integer);
      --% node.p_body_part()
   end PT;

   protected body PT is
      entry E (X : Integer) when True is
      begin
         null;
      end E;
      --% node.p_decl_part()

      entry E when True is
      begin
         null;
      end E;
      --% node.p_decl_part()

      entry F (for D in 1 .. 5) (X : Integer) when True is
      begin
         null;
      end F;
      --% node.p_decl_part()
   end PT;
begin
   null;
end Test;
