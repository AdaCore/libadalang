procedure Test is
   type Even is range 1 .. 100
      with Dynamic_Predicate => (
         Even --% node.p_referenced_decl()
         mod 2) = 0;

   subtype By_4 is Even
      with Dynamic_Predicate => (
         By_4 --% node.p_referenced_decl()
         mod 4) = 0;

   type By_8 is new Even
      with Dynamic_Predicate => (
         By_8 --% node.p_referenced_decl()
         mod 8) = 0;
begin
   null;
end Test;
