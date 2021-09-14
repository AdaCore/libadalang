procedure Test is

   function P (X : Integer) return Boolean;
   function P (X : Float) return Boolean;

   function P (X : Integer) return Boolean is
   begin return True; end;
   --% node.p_previous_part()

   function P (X : Float) return Boolean is separate;
   --% node.p_previous_part()

begin
   null;
end Test;
