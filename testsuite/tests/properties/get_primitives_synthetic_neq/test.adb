procedure Test is
   package P is
      type Vector is tagged null record;
      --% node.p_get_primitives()

      overriding
      function "=" (Left : Vector; Right : Vector) return Boolean;
   end P;
   package body P is
      overriding
      function "=" (Left : Vector; Right : Vector) return Boolean is (True);
   end P;
begin
   null;
end Test;
