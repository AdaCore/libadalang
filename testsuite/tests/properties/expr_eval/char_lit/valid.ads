--  style: non-ascii

package Valid is

   type Enum is ('A', 'B');

   C1 : constant Character := 'A';
   --% node.f_default_expr.p_eval_as_int

   C2 : constant Character := 'Z';
   --% node.f_default_expr.p_eval_as_int

   C3 : constant Character := C1;
   --% node.f_default_expr.p_eval_as_int

   E1 : constant Enum := 'A';
   --% node.f_default_expr.p_eval_as_int

   E2 : constant Enum := 'B';
   --% node.f_default_expr.p_eval_as_int

   function F1 return Character renames 'A';
   --% node.f_renames.f_renamed_object.p_eval_as_int

   function F2 return Enum renames 'A';
   --% node.f_renames.f_renamed_object.p_eval_as_int

   function F3 return Character renames 'Z';
   --% node.f_renames.f_renamed_object.p_eval_as_int

   function F4 return Enum renames 'B';
   --% node.f_renames.f_renamed_object.p_eval_as_int

   R1 : Character renames C1;
   --% node.f_renaming_clause.f_renamed_object.p_eval_as_int

   R2 : Enum renames E1;
   --% node.f_renaming_clause.f_renamed_object.p_eval_as_int

   WC1 : constant Wide_Character := '⺎';
   --% node.f_default_expr.p_eval_as_int

   WWC1 : constant Wide_Wide_Character := '𠀤';
   --% node.f_default_expr.p_eval_as_int

   function Fwc1 return Wide_Character renames '⺎';
   --% node.f_renames.f_renamed_object.p_eval_as_int

   function Fwwc1 return Wide_Wide_Character renames '𠀤';
   --% node.f_renames.f_renamed_object.p_eval_as_int

end Valid;
