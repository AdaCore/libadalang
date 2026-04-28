procedure Bodies is
   function My_Fun (I : Integer) return Integer is
   begin
      return I + 1;
   end;
   --% node.p_is_constant_object
begin
   null;
end Pack;
