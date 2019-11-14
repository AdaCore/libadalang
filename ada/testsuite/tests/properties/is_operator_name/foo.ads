--  Note: this test checks the behavior of the is_operator_name property on
--  invalid constructs, so this source file is intentionally illegal Ada.

package Foo is
   A : constant := "+";
   --% node.f_expr.p_is_operator_name

   B : constant := Baz."and";
   --% node.f_expr.p_is_operator_name

   C : constant := "+" (Quz)."or";
   --% node.f_expr.p_is_operator_name

   D : constant := "notanoperator";
   --% node.f_expr.p_is_operator_name
end Foo;
