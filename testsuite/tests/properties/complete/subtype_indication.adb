procedure Subtype_Indication is
   A : B;
   --% list(node.f_type_expr.f_name.p_complete)

   type Booboolean is new Boolean;
   task Booool is
      entry B;
   end Booool;

   B : B;
   --% list(node.f_type_expr.f_name.p_complete)
   C : Boo;
   --% list(node.f_type_expr.f_name.p_complete)

   type Bar is new Integer;
   task Barrr is
      entry B;
   end Barrr;

   D : B;
   --% list(node.f_type_expr.f_name.p_complete)
   E : Bo;
   --% list(node.f_type_expr.f_name.p_complete)
   F : Ba;
   --% list(node.f_type_expr.f_name.p_complete)

   procedure P (D: B;
  --% list(node[0].f_type_expr.f_name.p_complete)

   type T is record
      A : B;
      --% list(node.f_component_def.f_type_expr.f_name.p_complete)
   end record;

   type T (D : B) is null record;
   --% list(node.f_discriminants.f_discr_specs[0].f_type_expr.f_name.p_complete)

begin
   null;
end Subtype_Indication;
