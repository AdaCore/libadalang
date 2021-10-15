package Test is
   type A is (X, Y, Z);

   package Pkg is
      procedure A (P : A);
      --% node.p_unique_identifying_name
   end Pkg;
end Test;
