pragma Assertion_Level (L1);

procedure Test is
   A : Integer with Ghost => L1;
   --% node.p_is_ghost_code

   B : Integer;
   --% node.p_is_ghost_code
   pragma Ghost (L1);

   C : Integer;
   --% node.p_is_ghost_code
begin
   null;
end Test;
