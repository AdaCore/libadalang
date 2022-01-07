package body Pack is
   function My_Fun (I : Integer) return Integer is
   begin
      return I + 1;
   end;

   My_2 : Integer renames My_Fun (1);
   --% node.p_is_constant_object
end Pack;
