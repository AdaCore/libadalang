package body P is

   procedure X (F : Integer) is separate;

   package Nested is
      procedure Boo;
      --% node.p_next_part_for_decl()
   end Nested;

   package body Nested is separate;

end P;
