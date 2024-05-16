package body Pkg is
   --  Check that T'(0) resolve in aspect specification (we have visibility on
   --  the private part of T here, so we should consider all previous parts of
   --  T when looking for the Integer_Literal aspect).

   procedure P1 (A : T) with
      Pre => A = T'(0);
   pragma Test_Block;

   procedure P1 (A : T) is null;

   --  Integer_Literal aspect can be inherited (as well as for Real_Literal and
   --  String_Literal, not tested here), so we should also consider parent types
   --  when looking for it.

   procedure P2 (A : U) with
      Pre => A = U'(0);
   pragma Test_Block;

   procedure P2 (A : U) is null;
end Pkg;
