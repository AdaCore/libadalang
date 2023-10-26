procedure Test is
   type P is tagged null record;
   type T is new P with record
      X : access T;
      --% access_type = node.f_component_def.f_type_expr.f_type_decl
      --% access_type.f_type_def.f_subtype_indication.p_designated_type_decl
      --% access_type.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
   end record;

   type T_2 is new P with record
      X : access T_2 := T_2'Unrestricted_Access;
      --% access_type = node.f_component_def.f_type_expr.f_type_decl
      --% access_type.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
      --% access_type.f_type_def.f_subtype_indication.p_designated_type_decl
      --% node.f_default_expr.f_prefix.p_referenced_decl()
   end record;
begin
   null;
end Test;

