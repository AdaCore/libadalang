procedure Test is
   function Foo (A : Integer) return Float;


   A : Float :=
     Foo --% node.p_failsafe_referenced_decl(True)
      (12);

   B : Integer :=
     Foo --% node.p_failsafe_referenced_decl(True)
       (12);

   C : Integer :=
     Barz --% node.p_failsafe_referenced_decl(True)
       (12);

   D : Integer;
   --% node.p_defining_name.p_failsafe_referenced_decl()

   E : Integer :=
      F  --% node.p_failsafe_referenced_decl()
      ;

   G : Integer := D
      'Size  --% node.p_failsafe_referenced_decl()
      ;
begin
   null;
end Test;
