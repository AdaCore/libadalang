procedure Test_Protected is
   package A is
      protected B is
         procedure I;
         --% $node.p_next_part_for_decl()
      private
         C, D : Integer;
      end B;
      --% $node.p_next_part_for_decl()
   end A;

   package body A is
      protected body B is
         procedure I is
         begin
            null;
         end I;
      end B;
   end A;

begin
   null;
end Test_Protected;
