procedure Test is
   package Pkg is
      X, Y : Integer;
      --% node.f_ids[0].p_has_aspect("volatile")
      --% node.f_ids[1].p_has_aspect("volatile")
      pragma Volatile (X);
   end Pkg;

   package Pkg_2 is
      X, Y : Integer;
      --% node.f_ids[0].p_get_at_clause()
      --% node.f_ids[1].p_get_at_clause()
      for Y use at X'Address;
   end Pkg_2;

   package Pkg_3 is
      X, Y : Integer;
      --% node.f_ids[0].p_get_representation_clause("address")
      --% node.f_ids[1].p_get_representation_clause("address")
      for Y'Address use X'Address;
   end Pkg_3;
begin
   null;
end Test;
