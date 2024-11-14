procedure Test is
   package Pkg is
      type T is range -100 .. 100;

      overriding function "=" (A, B : T) return Boolean is (False);
   end Pkg;

   use Pkg;

   A, B : T := 0;
   C : Boolean;
begin
   C := "=" (A, B);
   --% node.f_expr.f_name.p_referenced_decl().p_is_predefined_operator
   C := "/=" (A, B);
   --% node.f_expr.f_name.p_referenced_decl().p_is_predefined_operator
   A := "+" (A, B);
   --% node.f_expr.f_name.p_referenced_decl().p_is_predefined_operator
end Test;
