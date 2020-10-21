procedure Test is
   function Foo (A : Integer) return Float;


   A : Float :=
     Foo --% $node.p_failsafe_referenced_def_name(True)
      (12);

   B : Integer :=
     Foo --% $node.p_failsafe_referenced_def_name(True)
       (12);

   C : Integer :=
     Barz --% $node.p_failsafe_referenced_def_name(True)
       (12);
begin
   null;
end Test;
