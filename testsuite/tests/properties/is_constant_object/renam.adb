package body Renam is
   procedure P is
      R4 : Integer renames R (A => 4);
      --% node.p_is_constant_object
      R5 renames R (A => 5);
      --% node.p_is_constant_object
      Uno : Integer := R4;
      --% node.p_is_constant_object
      Dos : constant Integer := R4 * 2;
      --% node.p_is_constant_object
   begin
      null;
   end P;

   function R (A : Integer) return Integer is
      B : Integer renames A;
      --% node.p_is_constant_object
      C renames A;
      --% node.p_is_constant_object
   begin
      return A - 3;
   end R;
end Renam;
