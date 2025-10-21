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

   --  Test all the possible names

   E : constant := "=";
   --% node.f_expr.p_is_operator_name

   F : constant := "/=";
   --% node.f_expr.p_is_operator_name

   G : constant := "<";
   --% node.f_expr.p_is_operator_name

   H : constant := "<=";
   --% node.f_expr.p_is_operator_name

   I : constant := ">";
   --% node.f_expr.p_is_operator_name

   J : constant := ">=";
   --% node.f_expr.p_is_operator_name

   K : constant := "and";
   --% node.f_expr.p_is_operator_name

   L : constant := "or";
   --% node.f_expr.p_is_operator_name

   M : constant := "xor";
   --% node.f_expr.p_is_operator_name

   N : constant := "abs";
   --% node.f_expr.p_is_operator_name

   O : constant := "*";
   --% node.f_expr.p_is_operator_name

   P : constant := "**";
   --% node.f_expr.p_is_operator_name

   Q : constant := "/";
   --% node.f_expr.p_is_operator_name

   R : constant := "mod";
   --% node.f_expr.p_is_operator_name

   S : constant := "rem";
   --% node.f_expr.p_is_operator_name

   T : constant := "+";
   --% node.f_expr.p_is_operator_name

   U : constant := "-";
   --% node.f_expr.p_is_operator_name

   V : constant := "&";
   --% node.f_expr.p_is_operator_name

   W : constant := "not";
   --% node.f_expr.p_is_operator_name
end Foo;
