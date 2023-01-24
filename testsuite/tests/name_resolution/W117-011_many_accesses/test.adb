procedure Test is
   A, B, C, D, E, F, G, H : aliased Natural := 0;

   --  We expect a resolution failure! (but we want it fast)
   V : constant Missing_Vector :=
      A'Access &
      B'Access &
      C'Access &
      D'Access &
      E'Access &
      F'Access &
      G'Access &
      H'Access;
   pragma Test_Statement;
begin
   null;
end Test;
